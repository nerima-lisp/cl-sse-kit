(in-package #:sse-kit/http-kit-client)

(defstruct (http-sse-subscription
             (:conc-name %subscription-)
             (:constructor %make-http-sse-subscription
                 (&key url client protocol-client on-error on-retry on-redirect
                       lock stop-semaphore request-timeout
                       request-timeout-p)))
  url
  client
  protocol-client
  on-error
  on-retry
  on-redirect
  lock
  stop-semaphore
  thread
  active-request-token
  request-timeout
  request-timeout-p
  (stopped-p nil)
  (finished-p nil)
  (error nil)
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
  "Stop the subscription, cancel its active request, and wake retry waits.

The active request uses the cl-http-kit/client cancellation token.
Transport cancellation latency depends on the active protocol."
  (let ((subscription (%check-subscription subscription))
        (protocol-client nil)
        (cancel-token nil)
        (wake-worker-p nil))
    (cl-concurrent-kit:with-lock-held
        ((%subscription-lock subscription))
      (unless (%subscription-stopped-p subscription)
        (setf (%subscription-stopped-p subscription) t
              wake-worker-p t))
      (setf protocol-client
            (%subscription-protocol-client subscription)
            cancel-token
            (%subscription-active-request-token subscription)))
    (when wake-worker-p
      (cl-concurrent-kit:signal-semaphore
       (%subscription-stop-semaphore subscription)))
    (unwind-protect
         (when cancel-token
           (http-kit/client:http-client-cancel-request cancel-token))
      (when protocol-client
        (sse-kit:stop-http-sse-client protocol-client reason)))
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
(defun %subscription-redirect-request
    (client current-request initial-uri response redirect-count)
  (let* ((policy (http-kit/client:http-client-redirect-policy client))
         (current-uri (http-kit:http-request-uri current-request))
         (location
           (http-kit:http-header-value
            (http-kit:http-response-headers response) "location"))
         (target (http-kit/client:resolve-http-uri current-uri location))
         (downgrade-p
           (and (string-equal (http-kit:http-uri-scheme current-uri) "https")
                (string-equal (http-kit:http-uri-scheme target) "http")))
         (remove-names '("cookie" "proxy-authorization")))
    (when (>= redirect-count
             (http-kit/client:http-redirect-policy-max-redirects policy))
      (error 'http-kit/client:http-redirect-limit-exceeded
             :message "The HTTP redirect policy exhausted its redirect limit."
             :operation :redirect
             :detail (list :uri current-uri :redirects redirect-count)
             :uri current-uri
             :redirects redirect-count))
    (when (and downgrade-p
               (not (http-kit/client:http-redirect-policy-allow-downgrade-p
                     policy)))
      (error 'http-kit/client:http-client-error
             :message "The redirect policy rejected an HTTPS to HTTP downgrade."
             :operation :redirect
             :detail target))
    (unless (or (http-kit/client:http-redirect-policy-preserve-authorization-p
                 policy)
                (http-kit/client:http-same-origin-p initial-uri target))
      (push "authorization" remove-names))
    (unless (http-kit/client:http-same-origin-p current-uri target)
      (setf remove-names
            (append '("host" "origin" "referer") remove-names)))
    (http-kit:make-http-request
     :method (http-kit:http-request-method current-request)
     :protocol-version (http-kit:http-request-protocol-version current-request)
     :uri target
     :headers
     (remove-if
      (lambda (header)
        (member (http-kit:http-header-name header) remove-names
                :test #'string-equal))
      (http-kit:http-request-headers current-request))
     :trailers (http-kit:http-request-trailers current-request)
     :body (http-kit:http-request-body current-request))))

(defun %request-sse-response (subscription parser)
  (let* ((client (%subscription-client subscription))
         (protocol-client (%subscription-protocol-client subscription))
         (initial-request
           (http-kit/client:http-client-request
            client "GET" (%subscription-url subscription)
            :headers (%transport-headers protocol-client)))
         (initial-uri (http-kit:http-request-uri initial-request))
         (timeout-p (%subscription-request-timeout-p subscription)))
    (block request-loop
      (loop with request = initial-request
            with redirect-count = 0
            do (let ((cancel-token
                       (http-kit/client:make-http-cancellation-token)))
                 (unless
                     (cl-concurrent-kit:with-lock-held
                         ((%subscription-lock subscription))
                       (unless (%subscription-stopped-p subscription)
                         (setf (%subscription-active-request-token subscription)
                               cancel-token)
                         t))
                   (return-from request-loop nil))
                 (unwind-protect
                      (let ((response-kind nil)
                            (response-condition nil)
                            (redirect-location nil))
                        (let ((options
                                (list
                                 :collect-body-p nil
                                 :cancel-token cancel-token
                                 :on-response-headers
                                 (lambda (status headers)
                                   (let* ((policy
                                            (http-kit/client:http-client-redirect-policy
                                             client))
                                          (location
                                            (http-kit:http-header-value
                                             headers "location"))
                                          (redirect-p
                                            (and location
                                                 (member
                                                  status
                                                  (http-kit/client:http-redirect-policy-statuses
                                                   policy)
                                                  :test #'=))))
                                     (if redirect-p
                                         (setf response-kind :redirect
                                               redirect-location location)
                                         (let ((condition
                                                 (%response-condition
                                                  status headers)))
                                           (cond
                                             ((eq condition :no-content)
                                              (setf response-kind :no-content))
                                             (condition
                                              (setf response-kind :invalid
                                                    response-condition condition)
                                              (handler-case
                                                (http-kit/client:http-client-cancel-request
                                                 cancel-token)
                                                (error (cancel-condition)
                                                  (setf response-condition
                                                    (%response-condition
                                                     status headers
                                                     cancel-condition)))))
                                             (t
                                              (setf response-kind :sse)))))))
                                 :on-body-chunk
                                 (lambda (chunk)
                                   (when (and (eq response-kind :sse)
                                              (not (%subscription-stopped-p*
                                                    subscription)))
                                     (sse-kit:feed-http-sse-parser parser chunk)
                                     (%sync-parser-state subscription parser))))))
                          (when timeout-p
                            (setf options
                                  (append options
                                          (list :timeout
                                                (%subscription-request-timeout
                                                 subscription)))))
                          (handler-case
                              (multiple-value-bind (response effective-request)
                                  (apply #'http-kit/client:http-client-send
                                         client request options)
                                (cond
                                  ((eq response-kind :redirect)
                                   (when (%subscription-on-redirect subscription)
                                     (funcall (%subscription-on-redirect
                                               subscription)
                                              redirect-location response))
                                   (when (%subscription-stopped-p* subscription)
                                     (return-from request-loop nil))
                                   (setf request
                                         (%subscription-redirect-request
                                          client
                                          (or effective-request request)
                                          initial-uri
                                          response
                                          redirect-count))
                                   (incf redirect-count))
                                  ((eq response-kind :invalid)
                                   (error response-condition))
                                  (t
                                   (return-from request-loop response))))
                            (http-kit/client:http-client-request-cancelled
                             (condition)
                             (cond
                               ((eq response-kind :invalid)
                                (error response-condition))
                               ((%subscription-stopped-p* subscription)
                                (return-from request-loop nil))
                               (t
                                (error condition))))
                            (error (condition)
                             (if (eq response-kind :invalid)
                                 (error response-condition)
                                 (error condition))))
                   (cl-concurrent-kit:with-lock-held
                       ((%subscription-lock subscription))
                     (when (eq (%subscription-active-request-token subscription)
                               cancel-token)
                       (setf (%subscription-active-request-token subscription)
                             nil)))))))))
))

(defun %response-condition (status headers &optional cancellation-condition)
  (let ((content-type (http-kit:http-header-value headers "content-type")))
    (cond
      ((not (<= 200 status 299))
       (make-condition 'sse-kit:sse-http-error
                       :message "An SSE response must have a 2xx HTTP status."
                       :operation :http-response
                       :detail (when cancellation-condition
                                 (list :cancellation-error cancellation-condition))
                       :status status
                       :headers headers))
      ((not (sse-kit:http-sse-content-type-valid-p content-type))
       (make-condition 'sse-kit:sse-http-error
                       :message "An SSE response must have a valid text/event-stream content type."
                       :operation :http-response
                       :detail (when cancellation-condition
                                 (list :cancellation-error cancellation-condition))
                       :status status
                       :headers headers))
      ((= status 204) :no-content))))

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
    (let ((condition
            (%response-condition
             (http-kit:http-response-status response)
             (http-kit:http-response-headers response))))
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

(defun %wait-for-retry (subscription delay)
  (cl-concurrent-kit:wait-on-semaphore
   (%subscription-stop-semaphore subscription)
   :timeout delay)
  (not (%subscription-stopped-p* subscription)))

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
      (setf (%subscription-finished-p subscription) t)))
  :finished)

(defun subscribe-http-sse
    (url on-event &key http-client retry-policy (retry-forever-p nil)
                         (initial-last-event-id "") on-error on-retry on-redirect
                         (timeout nil timeout-p))
  "Start an asynchronous SSE subscription using an existing HTTP client.

ON-EVENT receives SSE event objects on the subscription worker thread. The
HTTP client object is reused unchanged for every request. Status and
Content-Type are validated in the response-headers callback before any body
chunk reaches the SSE parser. Redirect hops use the client's redirect policy.
Stopping cancels the active request and wakes a worker waiting to reconnect."
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
            :stop-semaphore (cl-concurrent-kit:make-semaphore)
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
