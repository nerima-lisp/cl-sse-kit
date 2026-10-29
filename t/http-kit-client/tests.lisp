(in-package #:sse-kit/http-kit-client/test)
(defvar *https-test-trust-anchors* nil)

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

(defun %make-test-http-client
    (&optional on-request
               &key default-headers
               (redirect-policy (http-kit/client:make-http-redirect-policy))
               strict-transport-store auth-provider challenge-auth-provider
               (cookie-jar (http-kit/client:make-http-cookie-jar)) proxy)
  (http-kit/client:make-http-client
   :open-stream (http-kit/network:make-http-network-stream-opener)
   :close-stream #'http-kit/network:close-http-tcp-stream
   :tls-upgrade
   (http-kit/tls:make-http-tls-upgrader
    :verify :required
    :trust-anchors *https-test-trust-anchors*
    :alpn-protocols (quote ("http/1.1")))
   :cookie-jar cookie-jar
   :strict-transport-store strict-transport-store
   :alternative-service-store nil
   :default-timeout 5
   :default-headers default-headers
   :redirect-policy redirect-policy
   :auth-provider auth-provider
   :challenge-auth-provider challenge-auth-provider
   :proxy proxy
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
(defun %run-http-fixture
    (mode &key
          (initial-host "127.0.0.1") (initial-scheme "https")
          default-headers
          (redirect-policy (http-kit/client:make-http-redirect-policy))
          strict-transport-store auth-provider challenge-auth-provider
          (cookie-jar (http-kit/client:make-http-cookie-jar))
          (initial-last-event-id "")
          expect-callback-p retry-forever-p stop-on-retry-p proxy-p)
  (let ((subscription nil)
        (callback-signal (cl-concurrent-kit:make-semaphore))
        (callback-release (cl-concurrent-kit:make-semaphore))
        (callback-kind nil)
        (events nil)
        (errors nil)
        (request-count 0)
        (retry-count 0)
        (records nil)
        (request-schemes nil))
    (multiple-value-bind (ignored server-records)
        (%call-with-https-server
         mode
         (lambda (port)
           (let ((client
                  (%make-test-http-client
                   (lambda (request attempt)
                     (declare (ignore attempt))
                     (incf request-count)
                     (push (http-kit:http-uri-scheme
                            (http-kit:http-request-uri request))
                           request-schemes))
                   :default-headers default-headers
                   :proxy
                   (when proxy-p
                     (http-kit/client:make-http-proxy
                      :scheme :http
                      :host "127.0.0.1"
                      :port port))
                   :redirect-policy redirect-policy
                   :strict-transport-store strict-transport-store
                   :auth-provider auth-provider
                   :challenge-auth-provider challenge-auth-provider
                   :cookie-jar cookie-jar)))
             (setf subscription
                   (sse-kit/http-kit-client:subscribe-http-sse
                    (format nil "~A://~A:~D/events"
                            initial-scheme initial-host port)
                    (lambda (event)
                      (push event events)
                      (setf callback-kind :event)
                      (cl-concurrent-kit:signal-semaphore callback-signal)
                      (unless (cl-concurrent-kit:wait-on-semaphore
                               callback-release :timeout 5)
                        (error "The test did not release the event callback.")))
                    :http-client client
                    :retry-policy (%test-retry-policy)
                    :retry-forever-p retry-forever-p
                    :initial-last-event-id initial-last-event-id
                    :on-error
                    (lambda (condition)
                      (push condition errors)
                      (setf callback-kind :error)
                      (cl-concurrent-kit:signal-semaphore callback-signal))
                    :on-retry
                    (lambda (delay condition)
                      (declare (ignore delay condition))
                      (incf retry-count)
                      (when stop-on-retry-p
                        (setf callback-kind :retry)
                        (cl-concurrent-kit:signal-semaphore callback-signal)
                        (sse-kit/http-kit-client:stop-http-sse-subscription
                         subscription :test-retry-observed)))))
             (when (and expect-callback-p
                        (not (cl-concurrent-kit:wait-on-semaphore
                              callback-signal :timeout 5)))
               (error "The HTTP fixture produced no event or error callback."))
             (when (eq callback-kind :event)
               (sse-kit/http-kit-client:stop-http-sse-subscription
                subscription :test-event-received)
               (cl-concurrent-kit:signal-semaphore callback-release))
             (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                      subscription :timeout 5)
               (sse-kit/http-kit-client:stop-http-sse-subscription
                subscription :test-cleanup)
               (cl-concurrent-kit:signal-semaphore callback-release)
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (error "The HTTP fixture subscription did not finish.")))
             t)))
      (declare (ignore ignored))
      (setf records server-records))
    (values (nreverse events)
            (nreverse errors)
            request-count
            retry-count
            records
            subscription
            (nreverse request-schemes))))

(defun %call-with-https-server (mode thunk)
  (let ((directory nil)
        (certificate nil)
        (key nil)
        (process nil)
        (output nil)
        (error-output nil)
        (trust-anchor nil))
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
                  "-addext" "subjectAltName=IP:127.0.0.1,DNS:localhost"
                  "-keyout" key "-out" certificate)
            :output :string
            :error-output :string)
           (setf trust-anchor
                 (cl-tls-kit.x509:parse-certificate-der
                  (cl-tls-kit:pem-block-der
                   (first
                    (cl-tls-kit:pem-decode
                     (uiop:read-file-string certificate))))))
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
                        (let ((*https-test-trust-anchors* (list trust-anchor)))
                          (funcall thunk port))
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
               (loop repeat 2
                     do (unless (cl-concurrent-kit:wait-on-semaphore
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
              :to-be-truthy)))
  (it "does not retry event callback errors and records them on the subscription"
    (let ((subscription nil)
          (error-received (cl-concurrent-kit:make-semaphore))
          (reported-condition nil)
          (error-count 0)
          (retry-count 0)
          (request-count 0)
          (records nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "callback-error"
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
                        (error "callback failed"))
                      :http-client client
                      :retry-policy (%test-retry-policy)
                      :on-error (lambda (condition)
                                  (setf reported-condition condition)
                                  (incf error-count)
                                  (cl-concurrent-kit:signal-semaphore
                                   error-received))
                      :on-retry (lambda (delay condition)
                                  (declare (ignore delay condition))
                                  (incf retry-count))))
               (unless (cl-concurrent-kit:wait-on-semaphore
                        error-received :timeout 5)
                 (error "The callback error was not reported."))
               (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                        subscription :timeout 5)
                 (error "The subscription worker did not finish."))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (expect request-count :to-equalp 1)
      (expect retry-count :to-equalp 0)
      (expect error-count :to-equalp 1)
      (expect (length records) :to-equalp 1)
      (expect (typep reported-condition (quote simple-error)) :to-equalp t)
      (expect (eq (sse-kit/http-kit-client:http-sse-subscription-error
                   subscription)
                  reported-condition)
              :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-last-event-id
               subscription)
              :to-equalp "")
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)))
  (it "does not report a parser callback error after stop wins"
      (let ((subscription nil)
            (callback-release (cl-concurrent-kit:make-semaphore))
            (event-count 0)
            (error-count 0)
            (retry-count 0)
            (request-count 0)
            (records nil))
        (multiple-value-bind (ignored server-records)
            (%call-with-https-server
             "callback-error"
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
                          (incf event-count)
                          (unless (cl-concurrent-kit:wait-on-semaphore
                                   callback-release :timeout 5)
                            (error "The parser callback was not released."))
                          (sse-kit/http-kit-client:stop-http-sse-subscription
                           subscription :test-stop-before-error)
                          (error "callback failed after stop"))
                        :http-client client
                        :retry-policy (%test-retry-policy)
                        :on-error (lambda (condition)
                                    (declare (ignore condition))
                                    (incf error-count))
                        :on-retry (lambda (delay condition)
                                    (declare (ignore delay condition))
                                    (incf retry-count))))
                 (cl-concurrent-kit:signal-semaphore callback-release)
                 (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                          subscription :timeout 5)
                   (sse-kit/http-kit-client:stop-http-sse-subscription
                    subscription :test-timeout)
                   (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                            subscription :timeout 5)
                     (error "The stopped subscription did not finish."))
                   (error "The stopped subscription did not finish."))
                 t)))
          (declare (ignore ignored))
          (setf records server-records))
        (expect event-count :to-equalp 1)
        (expect error-count :to-equalp 0)
        (expect retry-count :to-equalp 0)
        (expect request-count :to-equalp 1)
        (expect (length records) :to-equalp 1)
        (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
                 subscription)
                :to-equalp t)
        (expect (sse-kit/http-kit-client:http-sse-subscription-error
                 subscription)
                :to-equalp nil)
        (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                 subscription)
                :to-equalp t)))
  (it "does not retry a parser line size-limit error"
    (multiple-value-bind
        (events errors request-count retry-count records subscription)
        (%run-http-fixture
         "oversized-line"
         :retry-forever-p t
         :stop-on-retry-p t
         :expect-callback-p t)
      (expect retry-count :to-equalp 0)
      (expect request-count :to-equalp 1)
      (expect (length events) :to-equalp 0)
      (expect (length errors) :to-equalp 1)
      (expect (typep (first errors) (quote sse-kit:sse-size-limit-exceeded))
              :to-equalp t)
      (expect (sse-kit:sse-size-limit-exceeded-kind (first errors))
              :to-equalp :line)
      (expect (length records) :to-equalp 1)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)))
  (it "does not retry an event-count error raised while finishing a CR-terminated block"
      (let ((subscription nil)
            (event-count 0)
            (errors nil)
            (error-count 0)
            (retry-count 0)
            (request-count 0))
        (multiple-value-bind (ignored records)
            (%call-with-https-server
             "oversized-final-cr"
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
                          (incf event-count))
                        :http-client client
                        :retry-policy (%test-retry-policy)
                        :retry-forever-p t
                        :on-error (lambda (condition)
                                    (push condition errors)
                                    (incf error-count))
                        :on-retry (lambda (delay condition)
                                    (declare (ignore delay condition))
                                    (incf retry-count)
                                    (sse-kit/http-kit-client:stop-http-sse-subscription
                                     subscription :unexpected-retry))))
                 (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                          subscription :timeout 30)
                   (sse-kit/http-kit-client:stop-http-sse-subscription
                    subscription :test-timeout)
                   (unless (sse-kit/http-kit-client:wait-http-sse-subscription
                            subscription :timeout 5)
                     (error "The EOF parser error subscription did not stop."))
                   (error "The EOF parser error subscription did not finish."))
                 t)))
          (declare (ignore ignored))
          (expect event-count :to-equalp 10000)
          (expect error-count :to-equalp 1)
          (expect retry-count :to-equalp 0)
          (expect request-count :to-equalp 1)
          (expect (length errors) :to-equalp 1)
          (expect (typep (first errors) (quote sse-kit:sse-size-limit-exceeded))
                  :to-equalp t)
          (expect (sse-kit:sse-size-limit-exceeded-kind (first errors))
                  :to-equalp :events)
          (expect (length records) :to-equalp 1))))
  (it "reconnects with Last-Event-ID from an id-only block"
    (multiple-value-bind
        (events errors request-count retry-count records subscription)
        (%run-http-fixture "id-only-event-id" :expect-callback-p t)
      (declare (ignore retry-count))
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-event-data (first events))
              :to-equalp "finished")
      (expect (length errors) :to-equalp 1)
      (expect request-count :to-equalp 2)
      (expect (length records) :to-equalp 2)
      (expect (not (search "\"last-event-id\"" (first records)))
              :to-equalp t)
      (expect (search "\"last-event-id\":\"42\"" (second records))
              :to-be-truthy)
      (expect (sse-kit/http-kit-client:http-sse-subscription-last-event-id
               subscription)
              :to-equalp "42")))
  (it "clears Last-Event-ID from an empty id field before reconnecting"
    (multiple-value-bind
        (events errors request-count retry-count records subscription)
        (%run-http-fixture "empty-event-id"
                           :initial-last-event-id "seed"
                           :expect-callback-p t)
      (declare (ignore retry-count))
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-event-data (first events))
              :to-equalp "finished")
      (expect (length errors) :to-equalp 1)
      (expect request-count :to-equalp 2)
      (expect (length records) :to-equalp 2)
      (expect (search "\"last-event-id\":\"seed\"" (first records))
              :to-be-truthy)
      (expect (not (search "\"last-event-id\"" (second records)))
              :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-last-event-id
               subscription)
              :to-equalp "")))
  (it "rejects responses with duplicate Content-Type headers"
    (multiple-value-bind
        (events errors request-count retry-count records subscription)
        (%run-http-fixture "duplicate-content-type" :expect-callback-p t)
      (declare (ignore retry-count))
      (expect (length events) :to-equalp 0)
      (expect (length errors) :to-equalp 1)
      (expect (sse-kit:sse-error-operation (first errors))
              :to-equalp :http-response)
      (expect (sse-kit:sse-http-error-status (first errors))
              :to-equalp 200)
      (expect request-count :to-equalp 1)
      (expect (length records) :to-equalp 1)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)))
  (it "does not follow a 300 response when policy includes it"
    (let ((redirect-policy
            (http-kit/client:make-http-redirect-policy
             :statuses (quote (300 301 302 303 307 308)))))
      (multiple-value-bind
          (events errors request-count retry-count records subscription)
          (%run-http-fixture "redirect-300"
                             :redirect-policy redirect-policy
                             :expect-callback-p t)
        (declare (ignore retry-count))
        (expect (length events) :to-equalp 0)
        (expect (length errors) :to-equalp 1)
        (expect (sse-kit:sse-error-operation (first errors))
                :to-equalp :http-response)
        (expect (sse-kit:sse-http-error-status (first errors))
                :to-equalp 300)
        (expect request-count :to-equalp 1)
        (expect (length records) :to-equalp 1)
        (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                 subscription)
                :to-equalp t))))
  (it "does not retain events in callback subscription parsers"
    (let* ((protocol-client
             (sse-kit:make-http-sse-client
              :url "https://example.test/events"
              :on-event (lambda (event) (declare (ignore event)))))
           (events nil)
           (parser
             (sse-kit/http-kit-client::%make-subscription-parser
              protocol-client
              (lambda (event) (push event events)))))
      (sse-kit:feed-http-sse-parser parser (format nil "data: one~%~%"))
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-parser-event-count parser) :to-equalp 1)
      (expect (sse-kit:http-sse-parser-events parser) :to-equalp nil)))
  (it "keeps the subscription lock available after a timed retry wait"
    (let ((subscription
            (sse-kit/http-kit-client::%make-http-sse-subscription
             :lock (cl-concurrent-kit:make-lock)
             :stop-semaphore (cl-concurrent-kit:make-semaphore))))
      (expect (sse-kit/http-kit-client::%wait-for-retry subscription 0.001d0)
              :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
               subscription)
              :to-equalp nil))))
(describe "FR-006 response headers and request cancellation"
  (it "rejects invalid final response status and content type without dispatch or reconnect"
    (dolist (case (list (list "invalid-status" 500)
                        (list "non-200-status" 202)
                        (list "invalid-content-type" 200)
                        (list "invalid-204-content-type" 204)))
      (multiple-value-bind (events errors request-count retry-count records subscription)
          (%run-http-fixture (first case) :expect-callback-p t)
        (expect (length events) :to-equalp 0)
        (expect (length errors) :to-equalp 1)
        (expect (sse-kit:sse-error-operation (first errors))
                :to-equalp :http-response)
        (expect (sse-kit:sse-http-error-status (first errors))
                :to-equalp (second case))
        (expect request-count :to-equalp 1)
        (expect retry-count :to-equalp 0)
        (expect (length records) :to-equalp 1)
        (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
                 subscription)
                :to-equalp t)
        (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                 subscription)
                :to-equalp t))))
  (it "ends at a valid 204 response without dispatching or reconnecting"
    (multiple-value-bind (events errors request-count retry-count records subscription)
        (%run-http-fixture "valid-204")
      (expect (length events) :to-equalp 0)
      (expect (length errors) :to-equalp 0)
      (expect request-count :to-equalp 1)
      (expect retry-count :to-equalp 0)
      (expect (length records) :to-equalp 1)
      (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
               subscription)
              :to-equalp t)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)))
  (it "validates the final response after following a redirect"
    (multiple-value-bind (events errors request-count retry-count records subscription)
        (%run-http-fixture "redirect-invalid-final" :expect-callback-p t)
      (expect (length events) :to-equalp 0)
      (expect (length errors) :to-equalp 1)
      (expect (sse-kit:sse-error-operation (first errors))
              :to-equalp :http-response)
      (expect (sse-kit:sse-http-error-status (first errors)) :to-equalp 200)
      (expect request-count :to-equalp 2)
      (expect retry-count :to-equalp 0)
      (expect (length records) :to-equalp 2)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t)))
  (it "keeps GET for every configured redirect status"
      (dolist (mode '("redirect-301" "redirect-302" "redirect-303"
                                     "redirect-307" "redirect-308"))
        (multiple-value-bind (events errors request-count retry-count records subscription)
            (%run-http-fixture mode :expect-callback-p t)
          (expect (length events) :to-equalp 1)
          (expect (sse-kit:http-sse-event-data (first events))
                  :to-equalp "followed")
          (expect (length errors) :to-equalp 0)
          (expect request-count :to-equalp 2)
          (expect retry-count :to-equalp 0)
          (expect (length records) :to-equalp 2)
          (dolist (record records)
            (expect (search "\"method\":\"GET\"" record)
                    :to-be-truthy))
          (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
                   subscription)
                  :to-equalp t))))
  (it "strips client credentials across origins even when authorization preservation is enabled"
      (let* ((default-headers
              (list (http-kit:make-http-header "AuThOrIzAtIoN" "test-auth-value")
                    (http-kit:make-http-header "cookie" "user=explicit")
                    (http-kit:make-http-header "proxy-authorization" "proxy-test-value")
                    (http-kit:make-http-header "x-api-key" "api-key-value")
                    (http-kit:make-http-header "x-auth-token" "auth-token-value")
                    (http-kit:make-http-header "x-user-credential" "custom-credential")
                    (http-kit:make-http-header "accept" "text/html")
                    (http-kit:make-http-header "cache-control" "max-age=60")
                    (http-kit:make-http-header "last-event-id" "default-id")))
             (redirect-policy
              (http-kit/client:make-http-redirect-policy
               :preserve-authorization-p t)))
        (multiple-value-bind (events errors request-count retry-count records subscription)
            (%run-http-fixture "redirect-cross-origin"
                               :default-headers default-headers
                               :redirect-policy redirect-policy
                               :expect-callback-p t)
          (expect (length events) :to-equalp 1)
          (expect (length errors) :to-equalp 0)
          (expect request-count :to-equalp 2)
          (expect retry-count :to-equalp 0)
          (expect (length records) :to-equalp 2)
          (dolist (header (list "authorization" "cookie" "proxy-authorization"
                            "x-api-key" "x-auth-token" "x-user-credential"))
            ;; The fixture connects directly, so cl-http-kit strips this
            ;; proxy-only header before the first hop.
            (unless (string= header "proxy-authorization")
              (expect (search (format nil "\"~A\":" header) (first records))
                      :to-be-truthy))
            (expect (search (format nil "\"~A\":" header) (second records))
                    :to-equalp nil))
          (expect (search "\"accept\":\"text/event-stream\"" (second records))
                  :to-be-truthy)
          (expect (search "\"cache-control\":\"no-cache\"" (second records))
                  :to-be-truthy)
          (expect (search "\"last-event-id\":" (second records))
                  :to-equalp nil)
          (expect (search "sid=loopback" (second records)) :to-equalp nil)
          (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
                   subscription)
                  :to-equalp t))))
  (it "sends Proxy-Authorization only to the first origin across a proxy redirect"
    (let ((default-headers
           (list (http-kit:make-http-header
                  "Proxy-Authorization" "proxy-test-value"))))
      (multiple-value-bind
          (events errors request-count retry-count records subscription)
          (%run-http-fixture
           "proxy-auth-redirect"
           :initial-host "origin-one.invalid"
           :initial-scheme "http"
           :default-headers default-headers
           :proxy-p t
           :expect-callback-p t)
        (declare (ignore retry-count))
        (expect (length events) :to-equalp 1)
        (expect (sse-kit:http-sse-event-data (first events))
                :to-equalp "proxy redirected")
        (expect (length errors) :to-equalp 0)
        (expect request-count :to-equalp 2)
        (expect (length records) :to-equalp 2)
        (expect (search "\"path\":\"http://origin-one.invalid:" (first records))
                :to-be-truthy)
        (expect (search "\"proxy-authorization\":\"proxy-test-value\""
                        (first records))
                :to-be-truthy)
        (expect (search "\"path\":\"http://origin-two.invalid/events\""
                        (second records))
                :to-be-truthy)
        (expect (search "\"proxy-authorization\":" (second records))
                :to-equalp nil)
        (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                 subscription)
                :to-equalp t))))
  (it "fails closed on a cross-origin redirect when an auth provider is configured"
      (let ((provider-calls 0))
        (multiple-value-bind
            (events errors request-count retry-count records subscription)
            (%run-http-fixture
             "redirect-cross-origin-terminal"
             :auth-provider
             (lambda (request)
               (declare (ignore request))
               (incf provider-calls)
               "test-auth-provider-value")
             :expect-callback-p t)
          (expect (length events) :to-equalp 0)
          (expect (length errors) :to-equalp 1)
          (expect (http-kit:http-error-operation (first errors))
                  :to-equalp :redirect)
          (expect provider-calls :to-equalp 1)
          (expect request-count :to-equalp 1)
          (expect retry-count :to-equalp 0)
          (expect (length records) :to-equalp 1)
          (expect (search "\"authorization\":" (first records))
                  :to-be-truthy)
          (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                   subscription)
                  :to-equalp t))))
  (it "rejects cross-origin redirects with a challenge auth provider before a target 401"
    (let ((provider-calls 0))
      (multiple-value-bind
          (events errors request-count retry-count records subscription)
          (%run-http-fixture
           "redirect-cross-origin-challenge"
           :challenge-auth-provider
           (lambda (request response challenges)
             (declare (ignore request response challenges))
             (incf provider-calls)
             "provider-test-credential")
           :expect-callback-p t)
        (expect (length events) :to-equalp 0)
        (expect (length errors) :to-equalp 1)
        (expect provider-calls :to-equalp 0)
        (expect request-count :to-equalp 1)
        (expect retry-count :to-equalp 0)
        (expect (length records) :to-equalp 1)
        (expect (search "\"path\":\"/events\"" (first records))
                :to-be-truthy)
        (dolist (record records)
          (expect (search "\"authorization\":" record)
                  :to-equalp nil))
        (expect (typep (first errors)
                       'http-kit/client:http-client-error)
                :to-equalp t)
        (when (typep (first errors) 'http-kit/client:http-client-error)
          (expect (http-kit:http-error-operation (first errors))
                  :to-equalp :redirect))
        (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                 subscription)
                :to-equalp t))))



  (it "applies the default and configured redirect limits and rejects HTTPS downgrade"
      (let ((default-policy
             (http-kit/client:make-http-redirect-policy))
            (configured-policy
             (http-kit/client:make-http-redirect-policy :max-redirects 1)))
        (dolist (case
                    (list (list "redirect-limit" 11 default-policy
                                'http-kit/client:http-redirect-limit-exceeded)
                          (list "redirect-configured-limit" 2 configured-policy
                                'http-kit/client:http-redirect-limit-exceeded)
                          (list "redirect-downgrade" 1 default-policy
                                'http-kit/client:http-client-error)))
          (multiple-value-bind (events errors request-count retry-count records subscription)
              (%run-http-fixture (first case)
                                 :redirect-policy (third case)
                                 :expect-callback-p t)
            (expect (length events) :to-equalp 0)
            (expect (length errors) :to-equalp 1)
            (expect (typep (first errors) (fourth case)) :to-equalp t)
            (expect (http-kit:http-error-operation (first errors))
                    :to-equalp :redirect)
            (expect request-count :to-equalp (second case))
            (expect retry-count :to-equalp 0)
            (expect (length records) :to-equalp (second case))
            (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                     subscription)
                    :to-equalp t)))))
  (it "treats a status excluded by redirect policy as an invalid terminal response"
      (let ((policy
             (http-kit/client:make-http-redirect-policy :statuses '(301 307))))
        (multiple-value-bind (events errors request-count retry-count records subscription)
            (%run-http-fixture "redirect-status-disabled"
                               :redirect-policy policy
                               :expect-callback-p t)
          (expect (length events) :to-equalp 0)
          (expect (length errors) :to-equalp 1)
          (expect (sse-kit:sse-error-operation (first errors))
                  :to-equalp :http-response)
          (expect (sse-kit:sse-http-error-status (first errors)) :to-equalp 302)
          (expect request-count :to-equalp 1)
          (expect retry-count :to-equalp 0)
          (expect (length records) :to-equalp 1)
          (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
                   subscription)
                  :to-equalp t))))
  (it "allows a configured HTTPS downgrade and suppresses cross-site SameSite cookies"
      (let* ((cookie-jar (http-kit/client:make-http-cookie-jar))
             (seed-response
              (http-kit:make-http-response
               :status 200
               :headers
               (list (http-kit:make-http-header
                      "set-cookie" "lax=seed; Path=/; SameSite=Lax")
                     (http-kit:make-http-header
                      "set-cookie" "strict=seed; Path=/; SameSite=Strict"))))
             (redirect-policy
              (http-kit/client:make-http-redirect-policy :allow-downgrade-p t)))
        (http-kit/client:http-cookie-jar-accept-response
         cookie-jar "https://localhost/" seed-response)
        (multiple-value-bind
            (events errors request-count retry-count records subscription request-schemes)
            (%run-http-fixture "redirect-downgrade-allowed"
                               :initial-host "localhost"
                               :cookie-jar cookie-jar
                               :redirect-policy redirect-policy
                               :expect-callback-p t)
          (expect (length events) :to-equalp 1)
          (expect (sse-kit:http-sse-event-data (first events)) :to-equalp "followed")
          (expect (length errors) :to-equalp 0)
          (expect request-count :to-equalp 2)
          (expect retry-count :to-equalp 0)
          (expect (length records) :to-equalp 2)
          (expect request-schemes :to-equalp '("https" "http"))
          (expect (search "\"method\":\"GET\"" (first records)) :to-be-truthy)
          (expect (search "\"method\":\"GET\"" (second records)) :to-be-truthy)
          (expect (search "lax=seed" (second records)) :to-equalp nil)
          (expect (search "strict=seed" (second records)) :to-equalp nil)
          (expect (search "lax=scheme" (second records)) :to-equalp nil)
          (expect (search "strict=scheme" (second records)) :to-equalp nil)
          (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
                   subscription)
                  :to-equalp t))))
  (it "upgrades an HTTP redirect target through the strict transport store"
    (let* ((store (http-kit/client:make-http-strict-transport-store))
           (response
             (http-kit:make-http-response
              :status 200
              :headers
              (list (http-kit:make-http-header
                     "strict-transport-security" "max-age=3600")))))
      (http-kit/client:http-strict-transport-store-note-response
       store "https://localhost/" response)
      (multiple-value-bind (events errors request-count retry-count records subscription)
          (%run-http-fixture "redirect-hsts"
                             :strict-transport-store store
                             :expect-callback-p t)
        (expect (length events) :to-equalp 1)
        (expect (length errors) :to-equalp 0)
        (expect request-count :to-equalp 2)
        (expect retry-count :to-equalp 0)
        (expect (length records) :to-equalp 2)
        (expect (sse-kit/http-kit-client:http-sse-subscription-stopped-p
                 subscription)
                :to-equalp t))))
  (it "stops an active request within a bounded interval"
    (let ((subscription nil)
          (received (cl-concurrent-kit:make-semaphore))
          (events nil)
          (request-count 0)
          (records nil)
          (finished-p nil))
      (multiple-value-bind (ignored server-records)
          (%call-with-https-server
           "active-stop"
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
                        (cl-concurrent-kit:signal-semaphore received))
                      :http-client client
                      :retry-policy (%test-retry-policy)))
               (unless (cl-concurrent-kit:wait-on-semaphore
                        received :timeout 5)
                 (error "The active SSE response was not dispatched."))
               (sse-kit/http-kit-client:stop-http-sse-subscription subscription)
               (setf finished-p
                     (not (null
                           (sse-kit/http-kit-client:wait-http-sse-subscription
                            subscription :timeout 2))))
               t)))
        (declare (ignore ignored))
        (setf records server-records))
      (expect finished-p :to-equalp t)
      (expect (length events) :to-equalp 1)
      (expect (sse-kit:http-sse-event-data (first events)) :to-equalp "active")
      (expect request-count :to-equalp 1)
      (expect (length records) :to-equalp 1)
      (expect (sse-kit/http-kit-client:http-sse-subscription-finished-p
               subscription)
              :to-equalp t))))

