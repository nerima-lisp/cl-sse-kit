(in-package #:sse-kit)

(export '(http-sse-session-heartbeat-interval
          http-sse-session-heartbeat-count
          http-sse-session-heartbeat-due-p
          send-http-sse-heartbeat))

(defstruct (http-sse-session
             (:constructor %make-http-sse-session
                 (&key response write-octets flush close max-output-bytes
                       heartbeat-interval heartbeat-last-at synchronize)))
  response
  write-octets
  flush
  close
  (closed-p nil)
  (bytes-written 0)
  (event-count 0)
  heartbeat-interval
  (heartbeat-count 0)
  heartbeat-last-at
  close-reason
  close-error
  max-output-bytes
  synchronize)

(defun %sse-session-monotonic-time (&optional now)
  (if (null now)
      (get-internal-real-time)
      (if (integerp now)
          now
          (%sse-protocol-error
           "A session time must be an integer internal time value."
           now))))

(defun %sse-default-session-response ()
  (http-message-kit:make-http-response
   :status 200
   :headers (list
             (http-message-kit:make-http-header
              "content-type" "text/event-stream; charset=utf-8")
             (http-message-kit:make-http-header
              "cache-control" "no-cache"))))

(defun make-http-sse-session
    (&key stream write-octets flush close
          (response (%sse-default-session-response))
          max-output-bytes heartbeat-interval synchronize)
  "Create an SSE session backed by a binary STREAM or an octet writer.

RESPONSE is an HTTP-MESSAGE-KIT:HTTP-RESPONSE with status 200 and an SSE
content type.  WRITE-OCTETS receives one complete octet vector per write.
HEARTBEAT-INTERVAL is measured in seconds when non-NIL.  SYNCHRONIZE, when
supplied, receives a thunk and must run it under the application's session
lock."
  (unless (http-message-kit:http-response-p response)
    (%sse-protocol-error "Expected an HTTP-MESSAGE-KIT:HTTP-RESPONSE value."
                         response))
  (unless (and (integerp (http-message-kit:http-response-status response))
               (= 200 (http-message-kit:http-response-status response)))
    (%sse-protocol-error
     "An SSE session response must use HTTP status 200."
     (http-message-kit:http-response-status response)))
  (unless (http-sse-content-type-valid-p
           (%sse-http-single-header-value
            (http-message-kit:http-response-headers response)
            "content-type"))
    (%sse-protocol-error
     "An SSE session response must use text/event-stream." response))
  (%sse-validate-limit max-output-bytes "MAX-OUTPUT-BYTES")
  (when (and heartbeat-interval
             (or (not (realp heartbeat-interval))
                 (not (>= heartbeat-interval 0))))
    (%sse-protocol-error
     "A heartbeat interval must be a non-negative real number."
     heartbeat-interval))
  (when (and stream write-octets)
    (%sse-protocol-error
     "Specify either a session stream or an octet writer, not both."))
  (unless (or stream write-octets)
    (%sse-protocol-error
     "An SSE session requires a stream or an octet writer."))
  (when (and stream (not (streamp stream)))
    (%sse-protocol-error "An SSE session stream must be a stream." stream))
  (when (and stream (not (%sse-binary-stream-p stream)))
    (%sse-protocol-error
     "An SSE session stream must accept octets."
     (stream-element-type stream)))
  (when (and write-octets (not (functionp write-octets)))
    (%sse-protocol-error
     "An SSE session octet writer must be a function."
     write-octets))
  (when (and flush (not (functionp flush)))
    (%sse-protocol-error "An SSE session flush function must be a function."
                         flush))
  (when (and close (not (functionp close)))
    (%sse-protocol-error "An SSE session close function must be a function."
                         close))
  (when (and synchronize (not (functionp synchronize)))
    (%sse-protocol-error
     "An SSE session synchronization function must be a function."
     synchronize))
  (%make-http-sse-session
   :response response
   :write-octets (or write-octets
                     (lambda (octets) (write-sequence octets stream)))
   :flush (or flush (and stream (lambda () (finish-output stream))))
   :close (or close (and stream (lambda () (close stream))))
   :max-output-bytes max-output-bytes
   :heartbeat-interval heartbeat-interval
   :heartbeat-last-at (%sse-session-monotonic-time)
   :synchronize synchronize))

(defun %sse-session-call (session thunk)
  (let ((synchronize (http-sse-session-synchronize session)))
    (if synchronize
        (funcall synchronize thunk)
        (funcall thunk))))

(defun http-sse-session-open-p (session)
  (and (http-sse-session-p session)
       (not (http-sse-session-closed-p session))))

(defun %sse-check-session-open (session)
  (unless (http-sse-session-p session)
    (%sse-protocol-error "Expected an HTTP-SSE-SESSION value." session))
  (when (http-sse-session-closed-p session)
    (%sse-protocol-error "An HTTP-SSE-SESSION is already closed." session))
  session)

(defun %sse-session-remaining-output-bytes (session)
  (let ((limit (http-sse-session-max-output-bytes session)))
    (and limit
         (- limit (http-sse-session-bytes-written session)))))

(defun %sse-session-handle-write-error (session condition)
  (unless (http-sse-session-closed-p session)
    (setf (http-sse-session-closed-p session) t
          (http-sse-session-close-reason session) condition)
    (%sse-session-run-close-callback session))
  (error condition))

(defun %sse-session-run-close-callback (session)
  (when (http-sse-session-close session)
    (handler-case
        (funcall (http-sse-session-close session))
      (condition (condition)
        (setf (http-sse-session-close-error session) condition))))
  session)

(defun %sse-session-write (session octets event-p)
  (%sse-check-session-open session)
  (handler-case
      (let ((limit (http-sse-session-max-output-bytes session))
            (observed (+ (http-sse-session-bytes-written session)
                         (length octets))))
        (when (and limit (> observed limit))
          (%sse-size-error
           "SSE session output exceeded MAX-OUTPUT-BYTES."
           limit observed :output-bytes))
        (funcall (http-sse-session-write-octets session) octets))
    (error (condition)
      (%sse-session-handle-write-error session condition)))
  (incf (http-sse-session-bytes-written session) (length octets))
  (when event-p
    (incf (http-sse-session-event-count session)))
  session)

(defun send-http-sse-event (session event)
  "Serialize EVENT and write it atomically through SESSION's octet callback."
  (%sse-session-call
   session
   (lambda ()
     (%sse-check-session-open session)
     (handler-case
         (%sse-session-write
          session
          (serialize-http-sse-event
           event
           :max-bytes (%sse-session-remaining-output-bytes session))
          t)
       (sse-size-limit-exceeded (condition)
         (%sse-session-handle-write-error session condition))))))

(defun %sse-comment-octets (comment &key max-bytes)
  (unless (and (stringp comment) (%sse-no-line-breaks-p comment))
    (%sse-protocol-error
     "An SSE comment must be a string without line breaks."
     comment))
  (%sse-validate-limit max-bytes "MAX-BYTES")
  (let* ((body-size (%sse-utf8-octet-length comment))
         (size (+ 1 body-size 4)))
    (when (and max-bytes (> size max-bytes))
      (%sse-size-error
       "Serialized SSE comment exceeds MAX-BYTES."
       max-bytes
       size
       :output-bytes))
    (let* ((prefix (%sse-utf8-octets ":"))
           (body (%sse-utf8-octets comment))
           (result (make-array size
                             :element-type '(unsigned-byte 8))))
      (replace result prefix)
      (replace result body :start1 (length prefix))
      (setf (aref result (+ (length prefix) (length body))) 13
            (aref result (+ (length prefix) (length body) 1)) 10
            (aref result (+ (length prefix) (length body) 2)) 13
            (aref result (+ (length prefix) (length body) 3)) 10)
      result)))

(defun %sse-session-send-comment (session comment)
  (%sse-check-session-open session)
  (handler-case
      (%sse-session-write
       session
       (%sse-comment-octets
        comment
        :max-bytes (%sse-session-remaining-output-bytes session))
       nil)
    (sse-size-limit-exceeded (condition)
      (%sse-session-handle-write-error session condition)))
  session)

(defun send-http-sse-comment (session comment)
  "Write COMMENT as an SSE comment block and return SESSION."
  (%sse-session-call
   session
   (lambda () (%sse-session-send-comment session comment))))

(defun http-sse-session-heartbeat-due-p (session &optional now)
  "Return true when SESSION's configured heartbeat interval has elapsed."
  (unless (http-sse-session-p session)
    (%sse-protocol-error "Expected an HTTP-SSE-SESSION value." session))
  (let ((interval (http-sse-session-heartbeat-interval session))
        (last-at (http-sse-session-heartbeat-last-at session)))
    (and (http-sse-session-open-p session)
         interval
         last-at
         (>= (- (%sse-session-monotonic-time now) last-at)
             (* interval internal-time-units-per-second)))))

(defun send-http-sse-heartbeat (session &optional now)
  "Write and flush an empty SSE comment as a heartbeat."
  (%sse-session-call
   session
   (lambda ()
     (%sse-check-session-open session)
     (let ((heartbeat-now (and now (%sse-session-monotonic-time now))))
       (%sse-session-send-comment session "")
       (%sse-session-flush session)
       (incf (http-sse-session-heartbeat-count session))
       (setf (http-sse-session-heartbeat-last-at session)
             (or heartbeat-now (%sse-session-monotonic-time)))
       session))))

(defun %sse-session-flush (session)
  (%sse-check-session-open session)
  (handler-case
      (when (http-sse-session-flush session)
        (funcall (http-sse-session-flush session)))
    (error (condition)
      (%sse-session-handle-write-error session condition)))
  session)

(defun flush-http-sse-session (session)
  "Flush SESSION when its transport supplies a flush operation."
  (%sse-session-call
   session
   (lambda () (%sse-session-flush session))))

(defun close-http-sse-session (session &optional reason)
  "Close SESSION idempotently and retain REASON for diagnostics."
  (unless (http-sse-session-p session)
    (%sse-protocol-error "Expected an HTTP-SSE-SESSION value." session))
  (%sse-session-call
   session
   (lambda ()
     (unless (http-sse-session-closed-p session)
       (setf (http-sse-session-closed-p session) t
             (http-sse-session-close-reason session) reason)
       (%sse-session-run-close-callback session))
     session)))

(defun send-http-sse-event/k (session event on-success on-error)
  (%sse-call/k (lambda () (send-http-sse-event session event))
               on-success
               on-error))

(defun send-http-sse-comment/k (session comment on-success on-error)
  (%sse-call/k (lambda () (send-http-sse-comment session comment))
               on-success
               on-error))

(defun flush-http-sse-session/k (session on-success on-error)
  (%sse-call/k (lambda () (flush-http-sse-session session))
               on-success
               on-error))

(defun close-http-sse-session/k (session on-success on-error &optional reason)
  (%sse-call/k (lambda () (close-http-sse-session session reason))
               on-success
               on-error))

(defmacro with-http-sse-session ((var &rest options) &body body)
  `(let ((,var (make-http-sse-session ,@options)))
     (unwind-protect
          (progn ,@body)
       (when (http-sse-session-open-p ,var)
         (close-http-sse-session ,var :scope-exit)))))
