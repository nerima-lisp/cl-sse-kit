(in-package #:sse-kit)

(export '(http-sse-publisher-max-history
          http-sse-publisher-max-queue
          http-sse-publisher-oldest-event-id
          http-sse-publisher-newest-event-id))

(defstruct (http-sse-publisher-entry
             (:constructor %make-http-sse-publisher-entry
                 (&key session replaying-p delivering-p queue queue-tail
                       queue-count)))
  session
  (replaying-p nil)
  (delivering-p nil)
  queue
  queue-tail
  (queue-count 0))

(defstruct (http-sse-publisher
             (:constructor %make-http-sse-publisher
                 (&key max-history max-queue synchronize)))
  max-history
  max-queue
  synchronize
  events
  sessions)

(defun %sse-publisher-call (publisher function)
  (funcall (http-sse-publisher-synchronize publisher) function))

(defun make-http-sse-publisher (&key (max-history 1000) (max-queue 1000)
                                     synchronize)
  "Create a bounded event publisher.

SYNCHRONIZE, when supplied, receives a thunk and must call it while holding
the application's publisher lock.  MAX-QUEUE bounds events waiting for a
session that is replaying or already delivering; an overflow closes that
session with an SSE-SIZE-LIMIT-EXCEEDED condition.  NIL disables this bound.
The default executes the thunk directly; this keeps locking policy in the
host server rather than in the protocol library."
  (unless (and (integerp max-history) (>= max-history 0))
    (%sse-protocol-error
     "A publisher history size must be a non-negative integer."
     max-history))
  (%sse-validate-limit max-queue "MAX-QUEUE")
  (when (and synchronize (not (functionp synchronize)))
    (%sse-protocol-error
     "A publisher synchronization function must be a function."
     synchronize))
  (%make-http-sse-publisher
   :max-history max-history
   :max-queue max-queue
   :synchronize (or synchronize (lambda (thunk) (funcall thunk)))))

(defun http-sse-publisher-history (publisher)
  "Return a snapshot of PUBLISHER's retained events, oldest first."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (%sse-publisher-call
   publisher
   (lambda ()
     (mapcar #'%sse-copy-http-sse-event
             (http-sse-publisher-events publisher)))))

(defun http-sse-publisher-oldest-event-id (publisher)
  "Return the event ID at the beginning of PUBLISHER's retained history."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (%sse-publisher-call
   publisher
   (lambda ()
     (let ((event (first (http-sse-publisher-events publisher))))
       (and event
            (http-sse-event-id event)
            (copy-seq (http-sse-event-id event)))))))

(defun http-sse-publisher-newest-event-id (publisher)
  "Return the event ID at the end of PUBLISHER's retained history."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (%sse-publisher-call
   publisher
   (lambda ()
     (let ((events (http-sse-publisher-events publisher)))
       (and events
            (http-sse-event-id (car (last events)))
            (copy-seq (http-sse-event-id (car (last events)))))))))

(defun http-sse-publisher-session-count (publisher)
  "Return the number of currently subscribed sessions."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (%sse-publisher-call
   publisher
   (lambda () (length (http-sse-publisher-sessions publisher)))))

(defun %sse-publisher-check-session (session)
  (unless (http-sse-session-p session)
    (%sse-protocol-error "Expected an HTTP-SSE-SESSION value." session))
  (unless (http-sse-session-open-p session)
    (%sse-protocol-error "An SSE publisher requires an open session." session))
  session)

(defun %sse-publisher-check-last-event-id (last-event-id)
  (unless (and (stringp last-event-id)
               (%sse-no-line-breaks-p last-event-id)
               (not (find #\Null last-event-id :test #'char=)))
    (%sse-protocol-error
     "A replay cursor must be a string without line breaks or NUL."
     last-event-id))
  last-event-id)

(defun %sse-publisher-retain-event (publisher event)
  (let ((max-history (http-sse-publisher-max-history publisher)))
    (when (plusp max-history)
      (let* ((events (append (http-sse-publisher-events publisher)
                             (list event)))
             (overflow (- (length events) max-history)))
        (setf (http-sse-publisher-events publisher)
              (if (plusp overflow)
                  (nthcdr overflow events)
                  events)))))
  publisher)

(defun %sse-publisher-find-entry (publisher session)
  (find session
        (http-sse-publisher-sessions publisher)
        :key #'http-sse-publisher-entry-session
        :test #'eq))

(defun %sse-publisher-remove-entry (publisher entry)
  (when (member entry (http-sse-publisher-sessions publisher) :test #'eq)
    (setf (http-sse-publisher-sessions publisher)
          (delete entry (http-sse-publisher-sessions publisher) :test #'eq))
    t))

(defun %sse-publisher-replay-snapshot (publisher last-event-id)
  (let ((events (copy-list (http-sse-publisher-events publisher))))
    (if (zerop (length last-event-id))
        events
        (let ((cursor (position last-event-id events
                                :from-end t
                                :test #'string=
                                :key (lambda (event)
                                       (or (http-sse-event-id event) "")))))
          (unless cursor
            (error 'sse-replay-unavailable
                   :message "The requested SSE replay cursor is unavailable."
                   :operation :sse-replay
                   :detail (list :last-event-id last-event-id)
                   :last-event-id last-event-id))
          (copy-list (nthcdr (1+ cursor) events))))))

(defun %sse-publisher-replay-events (publisher entry events)
  (let ((session (http-sse-publisher-entry-session entry)))
    (dolist (event events session)
      (unless (%sse-publisher-call
               publisher
               (lambda ()
                 (and (member entry (http-sse-publisher-sessions publisher)
                              :test #'eq)
                      (http-sse-session-open-p session))))
        (return-from %sse-publisher-replay-events session))
      (send-http-sse-event session event))))

(defun %sse-publisher-enqueue-event (publisher entry event)
  (let ((max-queue (http-sse-publisher-max-queue publisher)))
    (when (and max-queue
               (>= (http-sse-publisher-entry-queue-count entry)
                   max-queue))
      (return-from %sse-publisher-enqueue-event
        (values nil
                (%sse-make-size-error
                 "An SSE publisher session queue exceeded MAX-QUEUE."
                 max-queue
                  (1+ (http-sse-publisher-entry-queue-count entry))
                  :publisher-queue)))))
  (let ((cell (list event)))
    (if (http-sse-publisher-entry-queue entry)
        (setf (cdr (http-sse-publisher-entry-queue-tail entry)) cell
              (http-sse-publisher-entry-queue-tail entry) cell)
        (setf (http-sse-publisher-entry-queue entry) cell
              (http-sse-publisher-entry-queue-tail entry) cell))
    (incf (http-sse-publisher-entry-queue-count entry))
    (values t nil)))

(defun %sse-publisher-claim-next-event (publisher entry)
  (let ((session (http-sse-publisher-entry-session entry)))
    (cond
      ((not (member entry (http-sse-publisher-sessions publisher)
                   :test #'eq))
       (values nil nil))
      ((not (http-sse-session-open-p session))
       (%sse-publisher-remove-entry publisher entry)
       (values nil nil))
      ((http-sse-publisher-entry-queue entry)
       (setf (http-sse-publisher-entry-delivering-p entry) t)
       (let ((event (pop (http-sse-publisher-entry-queue entry))))
         (decf (http-sse-publisher-entry-queue-count entry))
         (when (null (http-sse-publisher-entry-queue entry))
           (setf (http-sse-publisher-entry-queue-tail entry) nil))
         (values event t)))
      (t
       (setf (http-sse-publisher-entry-delivering-p entry) nil)
       (values nil nil)))))

(defun %sse-publisher-drain-entry (publisher entry first-event)
  (let ((session (http-sse-publisher-entry-session entry))
        (event first-event)
        (delivered 0)
        (failures nil))
    (loop
      (if (not (%sse-publisher-call
               publisher
               (lambda ()
                 (and (member entry (http-sse-publisher-sessions publisher)
                              :test #'eq)
                      (http-sse-session-open-p session)))))
          (progn
            (%sse-publisher-call
             publisher
             (lambda () (%sse-publisher-remove-entry publisher entry)))
            (return))
          (handler-case
              (progn
                (send-http-sse-event session event)
                (incf delivered))
            (error (condition)
              (%sse-publisher-call
               publisher
               (lambda () (%sse-publisher-remove-entry publisher entry)))
              (close-http-sse-session session condition)
              (push (cons session condition) failures)
              (return))))
      (multiple-value-bind (next-event available-p)
          (%sse-publisher-call
           publisher
           (lambda () (%sse-publisher-claim-next-event publisher entry)))
        (unless available-p
          (return))
        (setf event next-event)))
    (values delivered (nreverse failures))))

(defun %sse-publisher-prepare-deliveries (publisher event)
  (let ((claims nil)
        (failures nil))
    (%sse-publisher-retain-event publisher event)
    (dolist (entry (copy-list (http-sse-publisher-sessions publisher)))
      (let ((session (http-sse-publisher-entry-session entry)))
        (cond
          ((not (http-sse-session-open-p session))
           (%sse-publisher-remove-entry publisher entry))
          ((or (http-sse-publisher-entry-replaying-p entry)
               (http-sse-publisher-entry-delivering-p entry))
           (multiple-value-bind (queued-p condition)
               (%sse-publisher-enqueue-event publisher entry event)
             (unless queued-p
               (%sse-publisher-remove-entry publisher entry)
               (push (cons session condition) failures))))
          (t
           (setf (http-sse-publisher-entry-delivering-p entry) t)
           (push (list entry event) claims)))))
    (values (nreverse claims) (nreverse failures))))

(defun %sse-publisher-finish-replay (publisher entry)
  (when (member entry (http-sse-publisher-sessions publisher) :test #'eq)
    (setf (http-sse-publisher-entry-replaying-p entry) nil)
    (%sse-publisher-claim-next-event publisher entry)))

(defun subscribe-http-sse-session
    (publisher session &key (last-event-id "") (replay-p t))
  "Subscribe SESSION and optionally replay events after LAST-EVENT-ID.

When LAST-EVENT-ID is empty, the retained history is replayed.  A non-empty
cursor must identify an event in the retained history or
SSE-REPLAY-UNAVAILABLE is signaled.  The session is registered before replay
so events published during replay are queued and delivered in order."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (%sse-publisher-check-session session)
  (%sse-publisher-check-last-event-id last-event-id)
  (unless (or (null replay-p) (eq replay-p t))
    (%sse-protocol-error "REPLAY-P must be a generalized boolean." replay-p))
  (let ((entry nil)
        (replay-events nil)
        (new-entry-p nil))
    (%sse-publisher-call
     publisher
     (lambda ()
       (unless (setf entry (%sse-publisher-find-entry publisher session))
         (when replay-p
           (setf replay-events
                 (%sse-publisher-replay-snapshot publisher last-event-id)))
         (setf entry
               (%make-http-sse-publisher-entry
                :session session
                :replaying-p replay-p))
         (push entry (http-sse-publisher-sessions publisher))
         (setf new-entry-p t))))
    (if (not new-entry-p)
        session
        (progn
          (handler-case
              (when replay-p
                (%sse-publisher-replay-events
                 publisher entry replay-events))
            (error (condition)
              (%sse-publisher-call
               publisher
               (lambda () (%sse-publisher-remove-entry publisher entry)))
              (close-http-sse-session session condition)
              (error condition)))
          (unless (http-sse-session-open-p session)
            (%sse-publisher-call
             publisher
             (lambda ()
               (%sse-publisher-remove-entry publisher entry)))
            (let ((reason (http-sse-session-close-reason session)))
              (error (if (typep reason 'condition)
                         reason
                         (make-condition
                          'sse-error
                          :message "An SSE session closed during replay."
                          :operation :sse-replay
                          :detail reason)))))
          (when replay-p
            (multiple-value-bind (first-event available-p)
                (%sse-publisher-call
                 publisher
                 (lambda () (%sse-publisher-finish-replay publisher entry)))
              (when available-p
                (multiple-value-bind (ignored failures)
                    (%sse-publisher-drain-entry publisher entry first-event)
                  (declare (ignore ignored))
                  (when failures
                    (error (cdar failures)))))))
          session))))

(defun unsubscribe-http-sse-session (publisher session)
  "Remove SESSION from PUBLISHER without closing the transport.

The operation remains valid after the transport has already closed, which
lets connection cleanup be idempotent."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (unless (http-sse-session-p session)
    (%sse-protocol-error "Expected an HTTP-SSE-SESSION value." session))
  (%sse-publisher-call
   publisher
   (lambda ()
     (let ((entry (%sse-publisher-find-entry publisher session)))
       (and entry (%sse-publisher-remove-entry publisher entry))))))

(defun publish-http-sse-event (publisher event)
  "Retain EVENT and deliver it to each open session.

Returns two values: the number of successful deliveries and an alist of
failed sessions and conditions.  Failed sessions are removed and closed so a
single broken transport cannot poison later broadcasts."
  (unless (http-sse-publisher-p publisher)
    (%sse-protocol-error "Expected an HTTP-SSE-PUBLISHER value." publisher))
  (unless (http-sse-event-p event)
    (%sse-protocol-error "Expected an HTTP-SSE-EVENT value." event))
  (let ((event (%sse-copy-http-sse-event event))
        (claims nil)
        (queued-failures nil)
        (delivered 0)
        (failures nil))
    (%sse-publisher-call
     publisher
     (lambda ()
       (multiple-value-setq (claims queued-failures)
         (%sse-publisher-prepare-deliveries publisher event))))
    (dolist (failure queued-failures)
      (let ((session (car failure))
            (condition (cdr failure)))
        (close-http-sse-session session condition)
        (setf failures (nconc failures (list failure)))))
    (dolist (claim claims)
      (destructuring-bind (entry first-event) claim
        (multiple-value-bind (count claim-failures)
            (%sse-publisher-drain-entry publisher entry first-event)
          (incf delivered count)
          (setf failures (nconc failures claim-failures)))))
    (values delivered failures)))
