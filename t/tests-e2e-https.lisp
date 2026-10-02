(in-package #:sse-kit/test)

#+sbcl
(progn
  (defun %e2e-read-headers (stream)
    (let ((bytes (make-array 0
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
      (loop for byte = (read-byte stream nil nil)
            do (unless byte
                 (error "The HTTPS E2E server reached EOF before headers."))
               (vector-push-extend byte bytes)
               (when (and (>= (length bytes) 4)
                          (= (aref bytes (- (length bytes) 4)) 13)
                          (= (aref bytes (- (length bytes) 3)) 10)
                          (= (aref bytes (- (length bytes) 2)) 13)
                          (= (aref bytes (- (length bytes) 1)) 10))
                 (return bytes)))))

  (defun %e2e-response (body)
    (let ((body-length (length body)))
      (concatenate '(vector (unsigned-byte 8))
                   (map '(vector (unsigned-byte 8)) #'char-code
                        (format nil
                                "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream; charset=utf-8~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
                                #\Return #\Linefeed
                                #\Return #\Linefeed
                                body-length
                                #\Return #\Linefeed
                                #\Return #\Linefeed
                                #\Return #\Linefeed))
                   body)))

  (defun %e2e-http-kit-headers (headers)
    (mapcar (lambda (header)
              (http-kit:make-http-header
               (http-message-kit:http-header-name header)
               (http-message-kit:http-header-content header)))
            headers))

  (defun %e2e-http-kit-sse-request (client)
    (http-kit:make-http-request
     :method "GET"
     :uri (http-sse-client-url client)
     :headers (%e2e-http-kit-headers
               (http-sse-client-request-headers client))))

  (defun %e2e-http-message-response (response)
    (cond
      ((http-kit:http-response-p response)
       (http-message-kit:make-http-response
        :status (http-kit:http-response-status response)
        :reason (http-kit:http-response-reason response)
        :headers (%e2e-http-message-headers
                  (http-kit:http-response-headers response))
        :trailers (%e2e-http-message-headers
                   (http-kit:http-response-trailers response))
        :body (http-kit:http-response-body response)
        :protocol-version (http-kit:http-response-protocol-version response)))
      ((http-kit:http-response-stream-p response)
       (http-message-kit:make-http-response-stream
        :status (http-kit:http-response-stream-status response)
        :reason (http-kit:http-response-stream-reason response)
        :headers (%e2e-http-message-headers
                  (http-kit:http-response-stream-headers response))
        :trailers (%e2e-http-message-headers
                   (http-kit:http-response-stream-trailers response))
        :body-function (http-kit:http-response-stream-body-function response)
        :body-length (http-kit:http-response-stream-body-length response)
        :protocol-version
        (http-kit:http-response-stream-protocol-version response)))))

  (defun %e2e-http-message-headers (headers)
    (mapcar (lambda (header)
              (http-message-kit:make-http-header
               (http-kit:http-header-name header)
               (http-kit:http-header-content header)))
            headers))

  (defun %e2e-start-server (openssl port certificate key responses requests)
    (let* ((process
             (uiop:launch-program
              (list openssl "s_server" "-quiet"
                    "-accept" (princ-to-string port)
                    "-cert" certificate "-key" key "-tls1_3"
                    "-alpn" "http/1.1")
              :input :stream
              :output :stream
              :error-output :stream
              :element-type '(unsigned-byte 8)
              :wait nil))
           (input (uiop:process-info-input process))
           (output (uiop:process-info-output process))
           (server-error nil)
           (server-thread nil))
      (sleep 1)
      (setf server-thread
            (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (dotimes (index (length responses))
                     (let* ((request (%e2e-read-headers output))
                            (request-text (octets-string request)))
                       (push request-text requests)
                       (write-sequence (%e2e-response (nth index responses)) input)
                       (finish-output input)))
                 (error (condition)
                   (setf server-error condition))))))
      (values process server-thread input output
              (lambda ()
                (when input
                  (ignore-errors (close input :abort t)))
                (when output
                  (ignore-errors (close output :abort t)))
                (when (and process (uiop:process-alive-p process))
                  (ignore-errors (uiop:terminate-process process)))
                (when process
                  (ignore-errors (uiop:wait-process process)))
                (when server-thread
                  (sb-thread:join-thread server-thread))
                (when server-error
                  (error server-error))))))

  (describe "HTTPS SSE client integration"
    (it "receives SSE fields and reconnects with Last-Event-ID"
      (let* ((directory (merge-pathnames "cl-sse-kit-https-e2e/"
                                        (uiop:temporary-directory)))
             (certificate (namestring (merge-pathnames "server.crt" directory)))
             (key (namestring (merge-pathnames "server.key" directory)))
             (openssl (or (uiop:getenv "OPENSSL") "openssl"))
             (port 18443)
             (responses
               (list
                (map '(vector (unsigned-byte 8)) #'char-code
                     (crlf ": keepalive" "event: update" "id: 1"
                            "retry: 25" "data: first" "data: line" ""))
                (map '(vector (unsigned-byte 8)) #'char-code
                     (crlf "event: update" "id: 2" "data: resumed" ""))))
             (requests nil)
             (process nil)
             (server-thread nil)
             (input nil)
             (output nil)
             (join-server nil))
        (ensure-directories-exist directory)
        (uiop:run-program
         (list openssl "req" "-x509" "-newkey" "rsa:2048" "-nodes"
               "-days" "1" "-subj" "/CN=127.0.0.1"
               "-addext" "subjectAltName=IP:127.0.0.1"
               "-keyout" key "-out" certificate)
         :output :string
         :error-output :string)
        (let ((trust-anchor
                (cl-tls-kit.x509:parse-certificate-der
                 (cl-tls-kit:pem-block-der
                  (first (cl-tls-kit:pem-decode
                          (uiop:read-file-string certificate)))))))
          (unwind-protect
               (progn
                 (multiple-value-setq
                     (process server-thread input output join-server)
                   (%e2e-start-server openssl port certificate key responses requests))
                 (let* ((client
                          (http-kit/client:make-http-client
                           :open-stream
                           (http-kit/network:make-http-network-stream-opener)
                           :close-stream #'http-kit/network:close-http-tcp-stream
                           :tls-upgrade
                           (http-kit/tls:make-http-tls-upgrader
                            :verify :required
                            :trust-anchors (list trust-anchor)
                            :alpn-protocols '("http/1.1"))
                           :strict-transport-store nil
                           :alternative-service-store nil))
                        (sse-client
                          (make-http-sse-client
                           :url (format nil "https://127.0.0.1:~D/events" port)
                           :retry-policy (test-retry-policy :max-attempts 1)))
                        (events nil))
                   (setf (http-sse-client-on-event sse-client)
                         (lambda (event) (push event events)))
                   (multiple-value-bind (response ignored)
                       (http-kit/client:http-client-send
                        client
                        (%e2e-http-kit-sse-request sse-client)
                        :timeout 5)
                     (declare (ignore ignored))
                     (consume-http-sse-client-response
                      sse-client
                      (%e2e-http-message-response response)))
                   (let ((event (first events)))
                     (expect (http-sse-event-event event) :to-equalp "update")
                     (expect (http-sse-event-data event)
                             :to-equalp (format nil "first~%line"))
                     (expect (http-sse-event-id event) :to-equalp "1"))
                   (expect (http-sse-client-last-event-id sse-client)
                           :to-equalp "1")
                   (expect (http-sse-client-retry-delay-override sse-client)
                           :to-equalp 0.025d0)
                   (setf events nil)
                     (multiple-value-bind (response ignored)
                         (http-kit/client:http-client-send
                          client
                          (%e2e-http-kit-sse-request sse-client)
                          :timeout 5)
                       (declare (ignore ignored))
                       (consume-http-sse-client-response
                        sse-client
                        (%e2e-http-message-response response)))
                     (let ((event (first events)))
                       (expect (http-sse-event-event event) :to-equalp "update")
                       (expect (http-sse-event-data event) :to-equalp "resumed")
                       (expect (http-sse-event-id event) :to-equalp "2"))
                     (expect (http-sse-client-last-event-id sse-client)
                             :to-equalp "2")
                     (funcall join-server)
                     (setf join-server nil)
                     (expect (some (lambda (request)
                                     (search "Last-Event-ID: 1" request))
                                   requests)
                             :to-equalp t)))
            (when join-server
              (ignore-errors (funcall join-server)))
            (when (and process
                       (not (uiop:process-alive-p process)))
              (ignore-errors (uiop:wait-process process)))
            (when (and process (uiop:process-alive-p process))
              (ignore-errors (uiop:terminate-process process)))))))))
