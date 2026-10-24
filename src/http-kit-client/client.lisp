(in-package #:sse-kit/http-kit-client)

(defstruct (http-sse-subscription
             (:conc-name %subscription-)
             (:constructor %make-http-sse-subscription
                 (&key url client protocol-client on-error on-retry on-redirect
                       lock condition-variable request-timeout
                       request-timeout-p)))
  url
  client
  protocol-client
  on-error
  on-retry
  on-redirect
  lock
  condition-variable
  thread
  request-timeout
  request-timeout-p
  (stopped-p nil)
  (finished-p nil)
  (error nil)
  stop-reason
  (last-event-id "")
  (next-retry-delay nil))

(defun %check-subscription (subscription)
  (unless (http-sse-subscription-p subscription)
    (error 'type-error
           :datum subscription
           :expected-type 'http-sse-subscription))
  subscription)

(defun %subscription-stopped-p* (subscription)
  (cl-concurrent-kit:with-lock-held
      ((%subscription-lock subscription))
    (%subscription-stopped-p subscription)))

(defun http-sse-subscription-stopped-p (subscription)
  (%subscription-stopped-p* (%check-subscription subscription)))

(defun http-sse-subscription-finished-p (subscription)
  (let ((subscription (%check-subscription subscription)))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (%subscription-finished-p subscription))))

(defun http-sse-subscription-error (subscription)
  (let ((subscription (%check-subscription subscription)))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (%subscription-error subscription))))

(defun http-sse-subscription-last-event-id (subscription)
  (let ((subscription (%check-subscription subscription)))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (%subscription-last-event-id subscription))))

(defun http-sse-subscription-next-retry-delay (subscription)
  (let ((subscription (%check-subscription subscription)))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (%subscription-next-retry-delay subscription))))

(defun %set-subscription-error (subscription condition)
  (cl-concurrent-kit:with-lock-held
      ((%subscription-lock subscription))
    (setf (%subscription-error subscription) condition))
  condition)

(defun stop-http-sse-subscription (subscription &optional (reason :stopped))
  "Request a permanent stop and wake a worker waiting before reconnecting.

A request already blocked in cl-http-kit/client cannot be closed through its
public API. Event dispatch stops at the next body chunk, and the worker exits
when that request completes or reaches its configured timeout."
  (let ((subscription (%check-subscription subscription))
        (protocol-client nil))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (unless (%subscription-stopped-p subscription)
        (setf (%subscription-stopped-p subscription) t
              (%subscription-stop-reason subscription) reason))
      (setf protocol-client
            (%subscription-protocol-client subscription))
      (cl-concurrent-kit:condition-broadcast
       (%subscription-condition-variable subscription)))
    (when protocol-client
      (sse-kit:stop-http-sse-client protocol-client reason))
    subscription))

(defun wait-http-sse-subscription (subscription &key (timeout nil timeout-p))
  "Wait for the worker to finish; return SUBSCRIPTION or NIL on timeout."
  (let* ((subscription (%check-subscription subscription))
         (thread (%subscription-thread subscription)))
    (when thread
      (if timeout-p
          (let ((timed-out (list :timed-out)))
            (unless (eq timed-out
                        (cl-concurrent-kit:join-thread
                         thread :default timed-out :timeout timeout))
              (return-from wait-http-sse-subscription subscription)))
          (cl-concurrent-kit:join-thread thread)))
    (when (http-sse-subscription-finished-p subscription)
      subscription)))

(defun %dispatch-subscription-event (subscription event)
  (let ((dispatch-p nil)
        (protocol-client (%subscription-protocol-client subscription)))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (unless (%subscription-stopped-p subscription)
        (setf dispatch-p t)))
    (when dispatch-p
      (multiple-value-prog1
          (handler-case
              (funcall (sse-kit:http-sse-client-on-event protocol-client) event)
            (error (condition)
              (error (quote %subscription-callback-error)
                     :condition condition)))
        (cl-concurrent-kit:with-lock-held
            ((%subscription-lock subscription))
          (setf (%subscription-last-event-id subscription)
                (sse-kit:http-sse-event-last-event-id event)
                (sse-kit:http-sse-client-last-event-id protocol-client)
                (sse-kit:http-sse-event-last-event-id event)))))))
(define-condition %subscription-callback-error (error)
  ((condition :initarg :condition
              :reader %subscription-callback-error-condition)))

(defun %sync-parser-state (subscription parser)
  (let* ((protocol-client (%subscription-protocol-client subscription))
         (retry-ms (sse-kit:http-sse-parser-retry parser)))
    (when retry-ms
      (cl-concurrent-kit:with-lock-held
          ((%subscription-lock subscription))
        (let* ((policy (sse-kit:http-sse-client-retry-policy protocol-client))
               (maximum (or (resilience-kit:retry-policy-max-delay policy)
                            31536000d0))
               (limit-ms (ceiling (* maximum 1000d0)))
               (delay (/ (coerce (min retry-ms limit-ms) (quote double-float))
                         1000d0)))
          (setf (sse-kit:http-sse-client-retry-delay-override protocol-client)
                delay))))
    subscription))

(defun %transport-headers (protocol-client)
  (let* ((last-event-id (sse-kit:http-sse-client-last-event-id protocol-client))
         (headers
           (list (http-kit:make-http-header "accept" "text/event-stream")
                 (http-kit:make-http-header "cache-control" "no-cache"))))
    (if (zerop (length last-event-id))
        headers
        (append
         headers
         (list (http-kit:make-http-header
                "last-event-id"
                (map 'string #'code-char
                     (http-kit/client:http-utf8-octets last-event-id))))))))

(defun %request-sse-response (subscription parser)
  (let* ((client (%subscription-client subscription))
         (protocol-client (%subscription-protocol-client subscription))
         (request
           (http-kit/client:http-client-request
            client "GET" (%subscription-url subscription)
            :headers (%transport-headers protocol-client)))
         (options
           (list :collect-body-p nil
                 :on-body-chunk
                 (lambda (chunk)
                   (unless (%subscription-stopped-p* subscription)
                     (sse-kit:feed-http-sse-parser parser chunk)
                     (%sync-parser-state subscription parser)))))
         (timeout-p (%subscription-request-timeout-p subscription)))
    (when timeout-p
      (setf options
            (append options
                    (list :timeout
                          (%subscription-request-timeout subscription)))))
    (apply #'http-kit/client:http-client-send client request options)))

(defun %response-condition (subscription response)
  (let* ((status (http-kit:http-response-status response))
         (headers (http-kit:http-response-headers response))
         (content-type (http-kit:http-header-value headers "content-type")))
    (cond
      ((= status 204) :no-content)
      ((/= status 200)
       (let ((location (http-kit:http-header-value headers "location")))
         (when (and location (member status '(301 302 303 307 308) :test #'=)
                    (%subscription-on-redirect subscription))
           (funcall (%subscription-on-redirect subscription)
                    location response)))
       (make-condition 'sse-kit:sse-http-error
                       :message "An SSE response must have HTTP status 200."
                       :operation :http-response
                       :status status
                       :headers headers))
      ((not (sse-kit:http-sse-content-type-valid-p content-type))
       (make-condition 'sse-kit:sse-http-error
                       :message "An SSE response must have a valid text/event-stream content type."
                       :operation :http-response
                       :status status
                       :headers headers)))))

(defun %make-disconnected-condition ()
  (make-condition 'sse-kit:sse-client-disconnected
                  :message "The SSE response ended before the subscription stopped."
                  :operation :response-finished
                  :detail :eof))

(defun %perform-subscription-attempt (subscription)
  (let* ((protocol-client (%subscription-protocol-client subscription))
         (callback-condition nil)
         (parser
           (%make-subscription-parser
            protocol-client
            (lambda (event)
              (handler-case
                  (%dispatch-subscription-event subscription event)
                (%subscription-callback-error (callback-error)
                  (setf callback-condition
                        (%subscription-callback-error-condition
                         callback-error))
                  (stop-http-sse-subscription
                   subscription :event-callback-error))))))
         (response nil))
    (handler-case
        (setf response (%request-sse-response subscription parser))
      (error (condition)
        (unless callback-condition
          (return-from %perform-subscription-attempt
            (values condition nil)))))
    (when callback-condition
      (%set-subscription-error subscription callback-condition)
      (when (%subscription-on-error subscription)
        (funcall (%subscription-on-error subscription)
                 callback-condition))
      (return-from %perform-subscription-attempt
        (values callback-condition t)))
    (when (%subscription-stopped-p* subscription)
      (return-from %perform-subscription-attempt (values nil t)))
    (let ((condition (%response-condition subscription response)))
      (cond
        ((eq condition :no-content)
         (stop-http-sse-subscription subscription :no-content)
         (values nil t))
        (condition
         (values condition nil))
        (t
         (handler-case
             (progn
               (sse-kit:finish-http-sse-parser parser)
               (%sync-parser-state subscription parser)
               (setf (sse-kit:http-sse-client-retry-attempt protocol-client) 0)
               (values (%make-disconnected-condition) nil))
           (error (condition)
             (values condition nil))))))))
(defun %make-subscription-parser (protocol-client on-event)
  (sse-kit:make-http-sse-parser
   :initial-last-event-id
   (sse-kit:http-sse-client-last-event-id protocol-client)
   :on-event on-event
   :collect-events-p nil))

(defun %retry-delay (protocol-client attempt decision)
  (let* ((policy (sse-kit:http-sse-client-retry-policy protocol-client))
         (maximum (or (resilience-kit:retry-policy-max-delay policy)
                      31536000d0))
         (delay (max (resilience-kit:compute-backoff-delay policy attempt)
                     (or (resilience-kit:retry-decision-delay-hint decision) 0d0)
                     (or (sse-kit:http-sse-client-retry-delay-override
                          protocol-client)
                         0d0))))
    (min (coerce maximum 'double-float)
         (max 0d0 (coerce delay 'double-float)))))

(defun %schedule-subscription-retry (subscription condition)
  (when (%subscription-stopped-p* subscription)
    (return-from %schedule-subscription-retry nil))
  (when (%subscription-on-error subscription)
    (funcall (%subscription-on-error subscription) condition))
  (when (%subscription-stopped-p* subscription)
    (return-from %schedule-subscription-retry nil))
  (let* ((protocol-client (%subscription-protocol-client subscription))
         (attempt (1+ (sse-kit:http-sse-client-retry-attempt protocol-client)))
         (policy (sse-kit:http-sse-client-retry-policy protocol-client)))
    (setf (sse-kit:http-sse-client-retry-attempt protocol-client) attempt)
    (multiple-value-bind (retry-p decision)
        (resilience-kit:retry-policy-should-retry-p
         policy attempt :condition condition)
      (when (and (not retry-p)
                 (sse-kit:http-sse-client-retry-forever-p protocol-client)
                 (resilience-kit:retry-policy-retry-safe-p policy)
                 (resilience-kit:retry-decision-retry-p decision))
        (setf retry-p t))
      (if (not retry-p)
          (progn
            (%set-subscription-error subscription condition)
            (stop-http-sse-subscription subscription :retry-exhausted)
            nil)
          (let ((delay (%retry-delay protocol-client attempt decision)))
            (cl-concurrent-kit:with-lock-held
                ((%subscription-lock subscription))
              (setf (%subscription-next-retry-delay subscription) delay))
            (when (%subscription-on-retry subscription)
              (funcall (%subscription-on-retry subscription)
                       delay condition))
            (not (%subscription-stopped-p* subscription)))))))

(defun %monotonic-seconds ()
  (/ (get-internal-real-time)
     (float internal-time-units-per-second 1d0)))

(defun %wait-for-retry (subscription delay)
  (let ((deadline (+ (%monotonic-seconds) delay))
        (lock (%subscription-lock subscription))
        (condition-variable
          (%subscription-condition-variable subscription)))
    (cl-concurrent-kit:with-lock-held (lock)
      (loop
        for remaining = (- deadline (%monotonic-seconds))
        while (and (not (%subscription-stopped-p subscription))
                   (plusp remaining))
        do (cl-concurrent-kit:condition-wait
            condition-variable lock :timeout remaining))
      (not (%subscription-stopped-p subscription)))))

(defun %subscription-loop (subscription)
  (loop until (%subscription-stopped-p* subscription)
        do (multiple-value-bind (condition terminal-p)
               (%perform-subscription-attempt subscription)
             (when terminal-p
               (return))
             (when condition
               (unless (%schedule-subscription-retry subscription condition)
                 (return))
               (let ((delay
                       (http-sse-subscription-next-retry-delay subscription)))
                 (unless (%wait-for-retry subscription delay)
                   (return)))))))

(defun %subscription-worker (subscription)
  (unwind-protect
       (handler-case
           (%subscription-loop subscription)
         (error (condition)
           (%set-subscription-error subscription condition)
           (stop-http-sse-subscription subscription :error)))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (setf (%subscription-finished-p subscription) t)
      (cl-concurrent-kit:condition-broadcast
       (%subscription-condition-variable subscription))))
  :finished)

(defun subscribe-http-sse
    (url on-event &key http-client retry-policy (retry-forever-p nil)
                         (initial-last-event-id "") on-error on-retry on-redirect
                         (timeout nil timeout-p))
  "Start an asynchronous SSE subscription using an existing HTTP client.

ON-EVENT receives SSE event objects on the subscription worker thread. The
HTTP client object is reused unchanged for every request. Response status and
headers are checked after the body callback has received all chunks, because
cl-http-kit/client exposes its final response hook after body completion."
  (unless (or (stringp url) (http-kit:http-uri-p url))
    (error 'type-error :datum url :expected-type '(or string http-kit:http-uri)))
  (unless (functionp on-event)
    (error 'type-error :datum on-event :expected-type 'function))
  (unless (http-kit/client:http-client-p http-client)
    (error 'type-error
           :datum http-client
           :expected-type 'http-kit/client:http-client))
  (dolist (callback (list on-error on-retry on-redirect))
    (when (and callback (not (functionp callback)))
      (error 'type-error :datum callback :expected-type '(or function null))))
  (let* ((subscription
           (%make-http-sse-subscription
            :url url
            :client http-client
            :on-error on-error
            :on-retry on-retry
            :on-redirect on-redirect
            :lock (cl-concurrent-kit:make-lock
                   :name "cl-sse-kit subscription")
            :condition-variable
            (cl-concurrent-kit:make-condition-variable
             :name "cl-sse-kit retry wait")
            :request-timeout timeout
            :request-timeout-p timeout-p))
         (protocol-client
           (sse-kit:make-http-sse-client
            :url url
            :retry-policy retry-policy
            :retry-forever-p retry-forever-p
            :initial-last-event-id initial-last-event-id
            :on-event
            on-event)))
    (setf (%subscription-protocol-client subscription) protocol-client
          (%subscription-last-event-id subscription) initial-last-event-id
          (%subscription-thread subscription)
          (cl-concurrent-kit:make-thread
           (lambda () (%subscription-worker subscription))
           :name "cl-sse-kit SSE subscription"))
    subscription))
