(in-package #:sse-kit/http-kit-client/test)

(defun %read-lines (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect line))

(defun %server-record-time (record)
  (let* ((marker (search "\"at\":" record))
         (start (+ marker 5))
         (end (position-if (lambda (character)
                             (member character '(#\, #\})))
                           record :start start)))
    (coerce (read-from-string (subseq record start end)) 'double-float)))

(defun %make-test-http-client (&optional on-request)
  (http-kit/client:make-http-client
   :open-stream (http-kit/network:make-http-network-stream-opener)
   :close-stream #'http-kit/network:close-http-tcp-stream
   :tls-upgrade
   (http-kit/tls:make-http-tls-upgrader
    :verify nil
    :alpn-protocols '("http/1.1"))
   :strict-transport-store nil
   :alternative-service-store nil
   :default-timeout 5
   :on-request on-request))

(defun %test-retry-policy ()
  (resilience-kit:make-retry-policy
   :max-attempts 3
   :initial-delay 0.01d0
   :multiplier 1d0
   :max-delay 1d0
   :jitter :none
   :retry-safe-p t
   :condition-classifier
   (lambda (condition attempt)
     (declare (ignore condition attempt))
     t)))

(defun %call-with-https-server (mode thunk)
  (let ((directory nil)
        (certificate nil)
        (key nil)
        (process nil)
        (output nil)
        (error-output nil))
    (unwind-protect
         (progn
           (setf directory
                 (uiop:ensure-directory-pathname
                  (string-trim '(#\Return #\Linefeed)
                               (uiop:run-program '("mktemp" "-d")
                                                 :output :string
                                                 :error-output :string)))
                 certificate (namestring (merge-pathnames "server.crt" directory))
                 key (namestring (merge-pathnames "server.key" directory)))
           (uiop:run-program
            (list "openssl" "req" "-x509" "-newkey" "rsa:2048" "-nodes"
                  "-days" "1" "-subj" "/CN=127.0.0.1"
                  "-addext" "subjectAltName=IP:127.0.0.1"
                  "-keyout" key "-out" certificate)
            :output :string
            :error-output :string)
           (setf process
                 (uiop:launch-program
                  (list "python3"
                        (namestring
                         (asdf:system-relative-pathname
                          "cl-sse-kit"
                          "t/http-kit-client/serve_https.py"))
                        mode certificate key)
                  :output :stream
                  :error-output :stream
                  :wait nil)
                 output (uiop:process-info-output process)
                 error-output (uiop:process-info-error-output process))
           (let* ((port
                    (handler-case
                      (parse-integer (read-line output))
                    (end-of-file ()
                      (let ((exit-status (uiop:wait-process process)))
                        (setf process nil)
                        (error "HTTPS fixture exited before reporting its port (~D):~%~{~A~%~}"
                               exit-status
                               (%read-lines error-output))))))
                  (value
                    (handler-case
                        (funcall thunk port)
                      (error (condition)
                        (when process
                          (ignore-errors (uiop:terminate-process process))
                          (ignore-errors (uiop:wait-process process))
                          (setf process nil))
                        (error "HTTPS fixture test failed:~%~A~%Requests:~%~{~A~%~}~%stderr:~%~{~A~%~}"
                               condition
                               (%read-lines output)
                               (%read-lines error-output)))))
                  (exit-status (uiop:wait-process process)))
             (setf process nil)
             (let ((records (%read-lines output)))
               (unless (zerop exit-status)
                 (error "HTTPS fixture failed: ~A"
                        (format nil "~{~A~%~}" (%read-lines error-output))))
               (values value records))))
      (when process
        (ignore-errors (uiop:terminate-process process))
        (ignore-errors (uiop:wait-process process)))
      (when output
        (ignore-errors (close output :abort t)))
      (when error-output
        (ignore-errors (close error-output :abort t)))
      (when directory
        (uiop:delete-directory-tree directory :validate t)))))

(describe "cl-http-kit client SSE integration"
  (it "dispatches events and reconnects with Last-Event-ID after the SSE retry delay"
    (let ((subscription nil)
          (ready (cl-concurrent-kit:make-semaphore))
          (received (cl-concurrent-kit:make-semaphore))
          (events nil)
          (retry-delays nil)
          (request-count 0)
          (records nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "reconnect"
           (lambda (port)
             (let ((client
                     (%make-test-http-client
                      (lambda (request attempt)
                        (declare (ignore request attempt))
                        (incf request-count)))))
               (setf subscription
                     (sse-kit/http-kit-client:subscribe-http-sse
                      (format nil "https://127.0.0.1:~D/events" port)
                      (lambda (event)
                        (push event events)
                        (when (string= (sse-kit:http-sse-event-data event)
                                       "second")
                          (unless (cl-concurrent-kit:wait-on-semaphore
                                   ready :timeout 5)
                            (error "Subscription startup was not released."))
                          (sse-kit/http-kit-client:stop-http-sse-subscription
                           subscription))
                        (cl-concurrent-kit:signal-semaphore received))
                      :on-error
                      (lambda (condition)
                        (unless (typep condition
                                       (quote sse-kit:sse-client-disconnected))
                          (format *error-output* "~&SSE subscription error: ~A~%" condition)))
                      :http-client client
                      :retry-policy (%test-retry-policy)
                      :on-retry (lambda (delay condition)
                                  (declare (ignore condition))
                                  (push delay retry-delays))))
               (cl-concurrent-kit:signal-semaphore ready)
               (dotimes (index 2)
                 (declare (ignore index))
                 (unless (cl-concurrent-kit:wait-on-semaphore
                          received :timeout 10)
                   (error "Timed out waiting for SSE events.")))
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (error "The subscription worker did not finish."))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (let ((events (nreverse events)))
        (expect (length events) :to-equalp 2)
        (expect (sse-kit:http-sse-event-event (first events)) :to-equalp "update")
        (expect (sse-kit:http-sse-event-data (first events)) :to-equalp "first")
        (expect (sse-kit:http-sse-event-id (first events)) :to-equalp "first-id")
        (expect (sse-kit:http-sse-event-retry (first events)) :to-equalp 250)
        (expect (sse-kit:http-sse-event-event (second events)) :to-equalp "finish")
        (expect (sse-kit:http-sse-event-data (second events)) :to-equalp "second")
        (expect (sse-kit:http-sse-event-id (second events)) :to-equalp "second-id"))
      (expect request-count :to-equalp 2)
      (expect (length records) :to-equalp 2)
      (expect (not (search "\"last-event-id\"" (first records))) :to-equalp t)
      (expect (search "\"last-event-id\":\"first-id\"" (second records))
              :to-be-truthy)
      (expect (>= (- (%server-record-time (second records))
                     (%server-record-time (first records)))
                  0.20d0)
              :to-equalp t)
      (expect (>= (first retry-delays) 0.25d0) :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-last-event-id
               subscription)
              :to-equalp "second-id")))

  (it "stops a pending retry without sending another request"
    (let ((subscription nil)
          (ready (cl-concurrent-kit:make-semaphore))
          (received (cl-concurrent-kit:make-semaphore))
          (retry-scheduled (cl-concurrent-kit:make-semaphore))
          (events nil)
          (request-count 0)
          (records nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "stop"
           (lambda (port)
             (let ((client
                     (%make-test-http-client
                      (lambda (request attempt)
                        (declare (ignore request attempt))
                        (incf request-count)))))
               (setf subscription
                     (sse-kit/http-kit-client:subscribe-http-sse
                      (format nil "https://127.0.0.1:~D/stop" port)
                      (lambda (event)
                        (push event events)
                        (cl-concurrent-kit:signal-semaphore received))
                      :on-error
                      (lambda (condition)
                        (unless (typep condition
                                       (quote sse-kit:sse-client-disconnected))
                          (format *error-output* "~&SSE subscription error: ~A~%" condition)))
                      :http-client client
                      :retry-policy (%test-retry-policy)
                      :on-retry (lambda (delay condition)
                                  (declare (ignore condition))
                                  (unless (>= delay 1d0)
                                    (error "SSE retry delay was not applied."))
                                  (cl-concurrent-kit:signal-semaphore
                                   retry-scheduled))))
               (cl-concurrent-kit:signal-semaphore ready)
               (unless (cl-concurrent-kit:wait-on-semaphore
                        received :timeout 5)
                 (error "Timed out waiting for the SSE event."))
               (unless (cl-concurrent-kit:wait-on-semaphore
                        retry-scheduled :timeout 5)
                 (error "The retry was not scheduled."))
               (sse-kit/http-kit-client:stop-http-sse-subscription subscription)
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (error "The stopped subscription worker did not finish."))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-event-data (first events)) :to-equalp "once")
      (expect request-count :to-equalp 1)
      (expect (length records) :to-equalp 1)
      (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
               subscription)
              :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)))

  (it "follows redirects and retains cookies in the supplied HTTP client"
    (let ((subscription nil)
          (ready (cl-concurrent-kit:make-semaphore))
          (received (cl-concurrent-kit:make-semaphore))
          (events nil)
          (request-count 0)
          (records nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "redirect"
           (lambda (port)
             (let ((client
                     (%make-test-http-client
                      (lambda (request attempt)
                        (declare (ignore request attempt))
                        (incf request-count)))))
               (setf subscription
                     (sse-kit/http-kit-client:subscribe-http-sse
                      (format nil "https://127.0.0.1:~D/redirect" port)
                      (lambda (event)
                        (push event events)
                        (unless (cl-concurrent-kit:wait-on-semaphore
                                 ready :timeout 5)
                          (error "Subscription startup was not released."))
                        (sse-kit/http-kit-client:stop-http-sse-subscription
                         subscription)
                        (cl-concurrent-kit:signal-semaphore received))
                      :on-error
                      (lambda (condition)
                        (unless (typep condition
                                       (quote sse-kit:sse-client-disconnected))
                          (format *error-output* "~&SSE subscription error: ~A~%" condition)))
                      :http-client client
                      :retry-policy (%test-retry-policy)))
               (cl-concurrent-kit:signal-semaphore ready)
               (unless (cl-concurrent-kit:wait-on-semaphore
                        received :timeout 10)
                 (error "Timed out waiting for the redirected SSE event."))
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (error "The redirected subscription worker did not finish."))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-event-event (first events))
              :to-equalp "redirected")
      (expect (sse-kit:http-sse-event-data (first events)) :to-equalp "followed")
      (expect request-count :to-equalp 2)
      (expect (length records) :to-equalp 2)
      (expect (search "\"path\":\"/redirect\"" (first records))
              :to-be-truthy)
      (expect (search "\"path\":\"/events\"" (second records))
              :to-be-truthy)
      (expect (search "\"cookie\":\"sid=loopback\"" (second records))
              :to-be-truthy)))
  (it "does not reconnect with an ID from an undispatched event"
    (let ((subscription nil)
          (ready (cl-concurrent-kit:make-semaphore))
          (received (cl-concurrent-kit:make-semaphore))
          (events nil)
          (request-count 0)
          (records nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "partial-id"
           (lambda (port)
             (let ((client
                     (%make-test-http-client
                      (lambda (request attempt)
                        (declare (ignore request attempt))
                        (incf request-count)))))
               (setf subscription
                     (sse-kit/http-kit-client:subscribe-http-sse
                      (format nil "https://127.0.0.1:~D/events" port)
                      (lambda (event)
                        (push event events)
                        (unless (cl-concurrent-kit:wait-on-semaphore
                                 ready :timeout 5)
                          (error "Subscription startup was not released."))
                        (sse-kit/http-kit-client:stop-http-sse-subscription
                         subscription)
                        (cl-concurrent-kit:signal-semaphore received))
                      :http-client client
                      :retry-policy (%test-retry-policy)))
               (cl-concurrent-kit:signal-semaphore ready)
               (unless (cl-concurrent-kit:wait-on-semaphore
                        received :timeout 10)
                 (error "Timed out waiting for the recovered SSE event."))
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (error "The stopped subscription worker did not finish."))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-event-data (first events)) :to-equalp "recovered")
      (expect (sse-kit:http-sse-event-id (first events)) :to-equalp "committed")
      (expect request-count :to-equalp 2)
      (expect (length records) :to-equalp 2)
      (expect (not (search "\"last-event-id\"" (second records))) :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-last-event-id
               subscription)
              :to-equalp "committed")))
  (it "respects the retry policy attempt limit by default"
    (let ((subscription nil)
          (retry-count 0)
          (request-count 0)
          (records nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "retry-limit"
           (lambda (port)
             (let ((client
                     (%make-test-http-client
                      (lambda (request attempt)
                        (declare (ignore request attempt))
                        (incf request-count)))))
               (setf subscription
                     (sse-kit/http-kit-client:subscribe-http-sse
                      (format nil "https://127.0.0.1:~D/events" port)
                      (lambda (event)
                        (declare (ignore event))
                        (error "The error response dispatched an event."))
                      :http-client client
                      :retry-policy (%test-retry-policy)
                      :on-retry (lambda (delay condition)
                                  (declare (ignore delay condition))
                                  (incf retry-count))))
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (sse-kit/http-kit-client:stop-http-sse-subscription
                  subscription)
                 (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                          subscription :timeout 5)
                   (error "The subscription worker did not stop after cleanup."))
                 (error "The subscription did not respect its retry limit."))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (expect request-count :to-equalp 3)
      (expect retry-count :to-equalp 2)
      (expect (length records) :to-equalp 3)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-error
               subscription)
              :to-be-truthy))))

(defun run-tests ()
  (unless (run-all :reporter :spec
                   :max-workers 1
                   :timeout-ms 300000
                   :pass-with-no-tests nil)
    (error "cl-sse-kit HTTP client integration tests failed."))
  t)
