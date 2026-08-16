(in-package #:sse-kit/test)

(defun test-response (&key (status 200)
                           (content-type "text/event-stream; charset=utf-8")
                           (headers nil))
  (http-message-kit:make-http-response
   :status status
   :headers (append
             (when content-type
               (list (http-message-kit:make-http-header
                      "content-type"
                      content-type)))
             headers)))

(defun test-retry-policy (&key (max-attempts 3) (initial-delay 2d0))
  (resilience-kit:make-retry-policy
   :max-attempts max-attempts
   :initial-delay initial-delay
   :multiplier 2d0
   :max-delay 10d0
   :jitter :none
   :retry-safe-p t
   :condition-classifier
   (lambda (condition attempt)
     (declare (ignore condition attempt))
     t)))

(defun test-header-value (headers name)
  (http-message-kit:http-header-value headers name))

(defun test-octets-string (octets)
  (map 'string #'code-char octets))

(defun test-http-date (timestamp)
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time timestamp 0)
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
            (nth weekday '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
            day
            (nth (1- month)
                 '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            year hour minute second)))

(defun test-request (&key (method "GET") (accept '("text/event-stream"))
                          body last-event-id)
  (http-message-kit:make-http-request
   :method method
   :uri "https://example.test/events"
   :headers (append
             (mapcar (lambda (value)
                       (http-message-kit:make-http-header "accept" value))
                     accept)
             (when last-event-id
               (list (http-message-kit:make-http-header
                      "last-event-id" last-event-id))))
   :body body))

(describe "HTTP content type grammar"
  (it "accepts only valid SSE content types"
    (dolist (case '(("text/event-stream" t)
                    ("text/event-stream; charset=utf-8" t)
                    ("text/event-stream; charset=\"utf-8\"; foo=bar" t)
                    ("text/event-stream; charset=iso-8859-1" t)
                    ("text/plain" nil)
                    ("text/event-stream;" nil)
                    ("text/event-stream; charset=utf-8; charset=utf-8" nil)
                    ("text/event-stream; =utf-8" nil)
                    ("text/event-stream; foo" nil)
                    ("text/event-stream; foo=" nil)
                    ("text/event-stream; bad@name=value" nil)
                    ("text/event-stream; charset=utf-8;" nil)))
      (destructuring-bind (value expected) case
        (expect (http-sse-content-type-valid-p value)
                :to-equalp expected)))))

(describe "HTTP negotiation and response contracts"
  (it "validates Accept ranges and request metadata"
    (dolist (case '( ("text/event-stream" t)
                     ("text/plain, text/event-stream;q=1.000" t)
                     ("text/event-stream;q=0" nil)
                     ("*/*;q=0, text/event-stream;q=0.5" t)
                     ("text/*;q=0, text/event-stream;q=1" t)
                     ("text/event-stream;q=0, */*;q=1" nil)
                     ("text/event-stream;q=1; q=0" nil)
                     ("text/event-stream;q=0." nil)
                     ("text/event-stream;q=0.000" nil)
                     ("text/event-stream;q=1." t)
                     ("text/event-stream;q=1.0000" nil)
                     ("text/event-stream; foo=\"unterminated" nil)))
      (destructuring-bind (value expected) case
        (expect (http-sse-request-valid-p
                 (test-request :accept (list value)))
                :to-equalp expected)))
    (expect (http-sse-request-valid-p (test-request :accept nil))
            :to-equalp t)
    (expect (http-sse-request-valid-p (test-request :method "POST"))
            :to-equalp nil)
    (let* ((request (test-request
                     :accept '("text/plain, text/event-stream;q=1.000")
                     :last-event-id "9"))
           (validated (validate-http-sse-request request)))
      (expect (eq validated request) :to-equalp t)
      (expect (http-sse-request-last-event-id request)
              :to-equalp "9"))
    (let* ((id-octets (cl-codec-kit:string-to-octets
                       "é-イベント"
                       :encoding :utf-8))
           (id-header (map 'string #'code-char id-octets)))
      (expect (http-sse-request-last-event-id
               (test-request :last-event-id id-header))
              :to-equalp "é-イベント")
      (expect (http-sse-request-last-event-id (test-request))
              :to-equalp ""))
    (let ((request
            (http-message-kit:make-http-request
             :method "GET"
             :uri "https://example.test/events"
             :headers (list
                       (http-message-kit:make-http-header
                        "accept" "text/event-stream")
                       (http-message-kit:make-http-header
                        "last-event-id" "1")
                       (http-message-kit:make-http-header
                        "last-event-id" "2")))))
      (expect (http-sse-request-valid-p request) :to-equalp nil)
      (let ((condition nil))
        (handler-case
            (validate-http-sse-request request)
          (sse-http-error (caught) (setf condition caught)))
        (expect (sse-http-error-status condition) :to-equalp 400)))
    (let ((condition nil))
      (handler-case
          (validate-http-sse-request (test-request :method "POST"))
        (sse-http-error (caught) (setf condition caught)))
      (expect (sse-http-error-status condition) :to-equalp 405)
      (expect (test-header-value (sse-http-error-headers condition) "allow")
              :to-equalp "GET"))
    (let ((condition nil)
          (body (make-array 1 :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (handler-case
          (validate-http-sse-request (test-request :body body))
        (sse-http-error (caught) (setf condition caught)))
      (expect (sse-http-error-status condition) :to-equalp 400))
    (let ((condition nil))
      (handler-case
          (validate-http-sse-request
           (test-request :accept '("application/json")))
        (sse-http-error (caught) (setf condition caught)))
      (expect (sse-http-error-status condition) :to-equalp 406)))

  (it "builds streaming responses with controlled headers"
    (let* ((chunk (cl-codec-kit:string-to-octets
                   (format nil "data: one~%~%")))
           (chunks (list chunk nil))
           (response
             (make-http-sse-response-stream
              :body-function (lambda () (pop chunks))
              :body-length (length chunk)
              :cors-origin "null"
              :cors-credentials-p t
              :x-accel-buffering t)))
      (expect (http-message-kit:http-response-stream-p response)
              :to-equalp t)
      (expect (http-message-kit:http-response-stream-status response)
              :to-equalp 200)
      (expect (test-header-value
               (http-message-kit:http-response-stream-headers response)
               "access-control-allow-origin")
              :to-equalp "null")
      (expect (test-header-value
               (http-message-kit:http-response-stream-headers response)
               "vary")
              :to-equalp "Origin")
      (expect (test-header-value
               (http-message-kit:http-response-stream-headers response)
               "access-control-allow-credentials")
              :to-equalp "true")
      (expect (test-header-value
               (http-message-kit:http-response-stream-headers response)
               "x-accel-buffering")
              :to-equalp "no")
      (expect (http-message-kit:http-response-stream-body-length response)
              :to-equalp (length chunk)))
    (signals sse-error
      (make-http-sse-response :cors-origin "*" :cors-credentials-p t))))

(describe "incremental parser protocol state"
  (it "keeps replay and retry state across chunk boundaries"
    (let* ((first-event
             (make-http-sse-event :id "7"
                                  :event "notice"
                                  :data "first"
                                  :retry 1250))
           (octets (serialize-http-sse-event first-event))
           (split (max 1 (floor (length octets) 2)))
           (parser (make-http-sse-parser))
           (events nil))
      (feed-http-sse-parser parser octets :end split)
      (feed-http-sse-parser parser octets :start split)
      (feed-http-sse-parser parser
                           (cl-codec-kit:string-to-octets
                           (format nil "data: second~%~%")))
      (finish-http-sse-parser parser)
      (setf events (http-sse-parser-events parser))
      (expect (length events) :to-equalp 2)
      (expect (http-sse-event-id (first events)) :to-equalp "7")
      (expect (http-sse-event-event (first events)) :to-equalp "notice")
      (expect (http-sse-event-data (first events)) :to-equalp "first")
      (expect (http-sse-event-retry (first events)) :to-equalp 1250)
      (expect (http-sse-event-data (second events)) :to-equalp "second")
      (expect (http-sse-parser-last-event-id parser) :to-equalp "7")
      (expect (http-sse-parser-retry parser) :to-equalp 1250)
      (expect (http-sse-parser-finished-p parser) :to-equalp t)))

  (it "does not dispatch an incomplete event at strict EOF"
    (let ((events (parse-http-sse-events "data: incomplete")))
      (expect events :to-equalp nil)))

  (it "preserves UTF-8 across octet chunk boundaries"
    (let* ((emoji (string (code-char #x1f600)))
           (octets (cl-codec-kit:string-to-octets
                    (format nil "data: ~A~%~%" emoji)))
           (split (1+ (length (cl-codec-kit:string-to-octets "data: "))))
           (parser (make-http-sse-parser)))
      (feed-http-sse-parser parser octets :end split)
      (feed-http-sse-parser parser octets :start split)
      (finish-http-sse-parser parser)
      (expect (http-sse-event-data
               (first (http-sse-parser-events parser)))
              :to-equalp emoji)))

  (it "reads character streams through the parser I/O boundary"
    (let ((events (read-http-sse-events
                   (make-string-input-stream
                    (format nil "data: stream~%~%")))))
      (expect (length events) :to-equalp 1)
      (expect (http-sse-event-data (first events)) :to-equalp "stream")))

  (it "seeds the parser cursor when reading a stream"
    (let ((events (read-http-sse-events
                   (make-string-input-stream
                    (format nil "data: resumed~%~%"))
                   :initial-last-event-id "resume")))
      (expect (http-sse-event-last-event-id (first events))
              :to-equalp "resume")))

  (it "seeds the parser cursor for one-shot parsing"
    (let ((events (parse-http-sse-events
                   (format nil "data: resumed~%~%")
                   :initial-last-event-id "resume")))
      (expect (http-sse-event-last-event-id (first events))
              :to-equalp "resume")))

  (it "supports parser CPS and scoped bindings"
    (let ((success nil)
          (failure nil)
          (parser (make-http-sse-parser)))
      (feed-http-sse-parser/k
       parser
       (format nil "data: cps~%~%")
       (lambda (value) (setf success value))
       (lambda (condition) (setf failure condition)))
      (expect success :to-equalp parser)
      (expect failure :to-equalp nil)
      (with-http-sse-parser (scoped-parser)
        (feed-http-sse-parser scoped-parser
                              (format nil "data: scoped~%~%"))
        (expect (http-sse-parser-finished-p scoped-parser) :to-equalp nil))
      (expect (http-sse-parser-finished-p parser) :to-equalp nil))))

(describe "HTTP message and session boundaries"
  (it "uses the direct HTTP response type and writes events"
    (let ((written nil)
          (flushed 0)
          (closed 0)
          (session nil))
      (setf session
            (make-http-sse-session
             :response (test-response :headers
                                      (list (http-message-kit:make-http-header
                                             "x-test" "yes")))
             :write-octets (lambda (octets)
                             (push (test-octets-string octets) written))
             :flush (lambda () (incf flushed))
             :close (lambda () (incf closed))))
      (expect (http-message-kit:http-response-status
               (http-sse-session-response session)) :to-equalp 200)
      (expect (test-header-value
               (http-message-kit:http-response-headers
                (http-sse-session-response session))
               "content-type")
              :to-equalp "text/event-stream; charset=utf-8")
      (expect (test-header-value
               (http-message-kit:http-response-headers
                (http-sse-session-response session))
               "x-test")
              :to-equalp "yes")
      (send-http-sse-event session
                           (make-http-sse-event :id "1" :data "hello"))
      (send-http-sse-comment session "heartbeat")
      (flush-http-sse-session session)
      (close-http-sse-session session :complete)
      (close-http-sse-session session :ignored)
      (expect (length written) :to-equalp 2)
      (expect flushed :to-equalp 1)
      (expect closed :to-equalp 1)
      (expect (http-sse-session-closed-p session) :to-equalp t)
      (expect (http-sse-session-close-reason session) :to-equalp :complete)))

  (it "enforces the response contract and output limits"
    (signals sse-error
      (make-http-sse-session
       :response (test-response :content-type "text/plain")
       :write-octets (lambda (octets) (declare (ignore octets)))))
    (signals sse-error
      (make-http-sse-session
       :stream (make-string-output-stream)))
    (signals sse-error
      (make-http-sse-session
       :response
       (test-response
        :content-type nil
        :headers (list
                  (http-message-kit:make-http-header
                   "content-type" "text/event-stream")
                  (http-message-kit:make-http-header
                   "content-type" "text/event-stream")))
       :write-octets (lambda (octets) (declare (ignore octets)))))
    (signals sse-error
      (let ((session
              (make-http-sse-session
               :max-output-bytes 4
               :write-octets (lambda (octets) (declare (ignore octets))))))
        (send-http-sse-event session
                             (make-http-sse-event :data "too long"))))
    (let ((session (make-http-sse-session
                    :write-octets (lambda (octets) (declare (ignore octets)))))
          (success nil)
          (failure nil))
      (send-http-sse-comment/k
       session
       "ok"
       (lambda (value) (setf success value))
       (lambda (condition) (setf failure condition)))
      (expect success :to-equalp session)
      (expect failure :to-equalp nil)
      (send-http-sse-comment/k
       session
       (format nil "bad~%comment")
       (lambda (value) (declare (ignore value)))
       (lambda (condition) (setf failure condition)))
      (expect (typep failure 'sse-error) :to-equalp t)
      (expect (http-sse-session-closed-p session) :to-equalp nil)
      (signals sse-error
        (send-http-sse-event session nil))
      (expect (http-sse-session-closed-p session) :to-equalp nil)
      (let ((limited-session
              (make-http-sse-session
               :max-output-bytes 8
               :write-octets (lambda (octets) (declare (ignore octets))))))
        (send-http-sse-comment limited-session "a")
        (signals sse-error
          (send-http-sse-comment limited-session "b"))
        (expect (http-sse-session-bytes-written limited-session)
                :to-equalp 6)
        (expect (http-sse-session-closed-p limited-session) :to-equalp t))
      (let* ((writes 0)
             (condition nil)
             (limited-session
               (make-http-sse-session
                :max-output-bytes 6
                :write-octets (lambda (octets)
                                (declare (ignore octets))
                                (incf writes)))))
        (handler-case
            (send-http-sse-comment limited-session "é")
          (sse-size-limit-exceeded (caught)
            (setf condition caught)))
        (expect (typep condition 'sse-size-limit-exceeded)
                :to-equalp t)
        (expect (sse-size-limit-exceeded-observed condition)
                :to-equalp 7)
        (expect writes :to-equalp 0)
        (expect (http-sse-session-bytes-written limited-session)
                :to-equalp 0))
      (close-http-sse-session session :test)))

  (it "tracks heartbeat deadlines and transport failures"
    (let ((flushed 0)
          (session nil))
      (setf session
            (make-http-sse-session
             :heartbeat-interval 1
             :write-octets (lambda (octets) (declare (ignore octets)))
             :flush (lambda () (incf flushed))))
      (let ((now (+ (http-sse-session-heartbeat-last-at session)
                    internal-time-units-per-second)))
        (expect (http-sse-session-heartbeat-due-p session now)
                :to-equalp t)
        (send-http-sse-heartbeat session now)
        (expect (http-sse-session-heartbeat-count session) :to-equalp 1)
        (expect flushed :to-equalp 1)
        (expect (http-sse-session-heartbeat-due-p session now)
                :to-equalp nil)))
    (let ((session
            (make-http-sse-session
             :write-octets (lambda (octets) (declare (ignore octets)))
             :flush (lambda () (error "flush failure")))))
      (signals error (flush-http-sse-session session))
      (expect (http-sse-session-closed-p session) :to-equalp t)))

  (it "serializes session mutations through host synchronization"
    (let ((depth 0)
          (max-depth 0)
          (calls 0)
          (session nil))
      (setf session
            (make-http-sse-session
             :synchronize
             (lambda (thunk)
               (incf calls)
               (incf depth)
               (setf max-depth (max max-depth depth))
               (unwind-protect
                    (funcall thunk)
                 (decf depth)))
             :write-octets (lambda (octets) (declare (ignore octets)))))
      (send-http-sse-heartbeat session)
      (send-http-sse-event session (make-http-sse-event :data "serialized"))
      (close-http-sse-session session :complete)
      (expect calls :to-equalp 3)
      (expect max-depth :to-equalp 1)))

  (it "closes scoped sessions on exit"
    (let ((session nil))
      (with-http-sse-session (value
                               :write-octets
                               (lambda (octets) (declare (ignore octets))))
        (setf session value))
      (expect (http-sse-session-closed-p session) :to-equalp t)
      (expect (http-sse-session-close-reason session) :to-equalp :scope-exit))))

(describe "publisher history and delivery"
  (it "replays retained events by cursor and synchronizes operations"
    (let ((synchronize-calls 0)
          (first-output nil)
          (second-output nil)
          (publisher nil)
          (first-session nil)
          (second-session nil))
      (setf publisher
            (make-http-sse-publisher
             :max-history 3
             :synchronize
             (lambda (thunk)
               (incf synchronize-calls)
               (funcall thunk))))
      (setf first-session
            (make-http-sse-session
             :write-octets (lambda (octets)
                             (push (test-octets-string octets) first-output))))
      (subscribe-http-sse-session publisher first-session)
      (publish-http-sse-event publisher
                               (make-http-sse-event :id "1" :data "one"))
      (publish-http-sse-event publisher
                               (make-http-sse-event :id "2" :data "two"))
      (setf second-session
            (make-http-sse-session
             :write-octets (lambda (octets)
                             (push (test-octets-string octets) second-output))))
      (subscribe-http-sse-session publisher second-session
                                  :last-event-id "1")
      (expect (length second-output) :to-equalp 1)
      (expect (not (null (search "data:two" (first second-output))))
              :to-equalp t)
      (publish-http-sse-event publisher
                               (make-http-sse-event :id "3" :data "three"))
      (publish-http-sse-event publisher
                               (make-http-sse-event :id "4" :data "four"))
      (expect (length (http-sse-publisher-history publisher)) :to-equalp 3)
      (signals sse-error
        (subscribe-http-sse-session
         publisher
         (make-http-sse-session
          :write-octets (lambda (octets) (declare (ignore octets))))
         :last-event-id "1"))
      (expect (> synchronize-calls 0) :to-equalp t)
      (expect (unsubscribe-http-sse-session publisher first-session)
              :to-equalp t)
      (expect (unsubscribe-http-sse-session publisher first-session)
              :to-equalp nil)))

  (it "isolates retained events from caller and history snapshots"
    (let* ((publisher (make-http-sse-publisher :max-history 2))
           (event (make-http-sse-event :id (copy-seq "1") :data "before"))
           (history nil))
      (publish-http-sse-event publisher event)
      (setf (http-sse-event-data event) "changed"
            (aref (http-sse-event-id event) 0) #\9)
      (expect (http-sse-event-id event) :to-equalp "9")
      (setf history (http-sse-publisher-history publisher))
      (expect (http-sse-event-data (first history)) :to-equalp "before")
      (expect (http-sse-event-id (first history)) :to-equalp "1")
      (setf (http-sse-event-data (first history)) "returned-change")
      (expect (http-sse-event-data
               (first (http-sse-publisher-history publisher)))
              :to-equalp "before")))

  (it "uses the latest duplicate cursor and reports history bounds"
    (let ((publisher (make-http-sse-publisher :max-history 4))
          (output nil)
          (session nil))
      (dolist (data '("old" "latest" "new"))
        (publish-http-sse-event
         publisher
         (make-http-sse-event :id (if (string= data "new") "new" "same")
                              :data data)))
      (expect (http-sse-publisher-oldest-event-id publisher)
              :to-equalp "same")
      (expect (http-sse-publisher-newest-event-id publisher)
              :to-equalp "new")
      (expect (http-sse-publisher-max-history publisher)
              :to-equalp 4)
      (setf session
            (make-http-sse-session
             :write-octets (lambda (octets)
                             (push (test-octets-string octets) output))))
      (subscribe-http-sse-session publisher session :last-event-id "same")
      (expect (length output) :to-equalp 1)
      (expect (not (null (search "data:new" (first output))))
              :to-equalp t)))

  (it "closes a session when replay delivery fails"
    (let* ((publisher (make-http-sse-publisher))
           (session (make-http-sse-session
                     :write-octets
                     (lambda (octets)
                       (declare (ignore octets))
                       (error "replay failure")))))
      (publish-http-sse-event publisher
                              (make-http-sse-event :id "1" :data "one"))
      (signals error (subscribe-http-sse-session publisher session))
      (expect (http-sse-session-closed-p session) :to-equalp t)
      (expect (http-sse-publisher-session-count publisher) :to-equalp 0)))

  (it "removes sessions closed by their replay writer"
    (let* ((publisher (make-http-sse-publisher))
           (session nil))
      (publish-http-sse-event publisher
                              (make-http-sse-event :id "1" :data "one"))
      (setf session
            (make-http-sse-session
             :write-octets
             (lambda (octets)
               (declare (ignore octets))
               (close-http-sse-session session :peer-closed))))
      (signals sse-error (subscribe-http-sse-session publisher session))
      (expect (http-sse-session-closed-p session) :to-equalp t)
      (expect (http-sse-publisher-session-count publisher) :to-equalp 0)))

  (it "removes sessions that fail during delivery"
    (let ((publisher (make-http-sse-publisher :max-history 2))
          (failure-session
            (make-http-sse-session
             :write-octets
             (lambda (octets)
               (declare (ignore octets))
               (error "delivery failure")))))
      (subscribe-http-sse-session publisher failure-session :replay-p nil)
      (multiple-value-bind (delivered failures)
          (publish-http-sse-event publisher
                                   (make-http-sse-event :data "failure"))
        (expect delivered :to-equalp 0)
        (expect (length failures) :to-equalp 1))
      (expect (http-sse-session-closed-p failure-session) :to-equalp t)
      (expect (http-sse-publisher-session-count publisher) :to-equalp 0)))

  (it "bounds queued delivery and reports queue overflow"
    (let* ((publisher (make-http-sse-publisher :max-queue 1))
           (entered-p nil)
           (overflow-failures nil)
           (session nil))
      (setf session
            (make-http-sse-session
             :write-octets
             (lambda (octets)
               (declare (ignore octets))
               (unless entered-p
                 (setf entered-p t)
                 (publish-http-sse-event
                  publisher
                  (make-http-sse-event :data "queued-1"))
                 (multiple-value-bind (ignored failures)
                     (publish-http-sse-event
                      publisher
                      (make-http-sse-event :data "queued-2"))
                   (declare (ignore ignored))
                   (setf overflow-failures failures))))))
      (subscribe-http-sse-session publisher session :replay-p nil)
      (multiple-value-bind (delivered failures)
          (publish-http-sse-event
           publisher
           (make-http-sse-event :data "first"))
        (expect delivered :to-equalp 1)
        (expect failures :to-equalp nil))
      (expect (length overflow-failures) :to-equalp 1)
      (expect (typep (cdr (first overflow-failures))
                     'sse-size-limit-exceeded)
              :to-equalp t)
      (expect (sse-size-limit-exceeded-kind
               (cdr (first overflow-failures)))
              :to-equalp :publisher-queue)
      (expect (http-sse-session-closed-p session) :to-equalp t)
      (expect (http-sse-publisher-session-count publisher) :to-equalp 0)))

  (it "does not deliver a claimed event after unsubscribe"
    (let* ((publisher (make-http-sse-publisher))
           (second-output nil)
           (second-session
             (make-http-sse-session
              :write-octets
              (lambda (octets)
                (push (test-octets-string octets) second-output))))
           (first-session
             (make-http-sse-session
              :write-octets
              (lambda (octets)
                (declare (ignore octets))
                (unsubscribe-http-sse-session publisher second-session)))))
      (subscribe-http-sse-session publisher second-session :replay-p nil)
      (subscribe-http-sse-session publisher first-session :replay-p nil)
      (multiple-value-bind (delivered failures)
          (publish-http-sse-event publisher
                                   (make-http-sse-event :data "first"))
        (expect delivered :to-equalp 1)
        (expect failures :to-equalp nil))
      (expect second-output :to-equalp nil)
      (expect (http-sse-publisher-session-count publisher) :to-equalp 1)))

  (it "stops replay when the session unsubscribes during replay"
    (let* ((publisher (make-http-sse-publisher))
           (output nil)
           (writes 0)
           (session nil))
      (loop repeat 2
            do (publish-http-sse-event
                publisher
                (make-http-sse-event :data "history")))
      (setf session
            (make-http-sse-session
             :write-octets
             (lambda (octets)
               (push (test-octets-string octets) output)
               (incf writes)
               (when (= writes 1)
                 (unsubscribe-http-sse-session publisher session)))))
      (subscribe-http-sse-session publisher session)
      (expect (length output) :to-equalp 1)
      (expect (http-sse-publisher-session-count publisher) :to-equalp 0))))

(describe "client response lifecycle"
  (it "uses response headers and applies parser retry metadata"
    (let ((events nil)
          (opened 0)
          (closed nil)
          (retries nil)
          (client nil))
      (setf client
            (make-http-sse-client
             :retry-policy (test-retry-policy)
             :on-event (lambda (event) (push event events))
             :on-open (lambda (response)
                        (declare (ignore response))
                        (incf opened))
             :on-close (lambda (reason) (push reason closed))
             :on-retry (lambda (delay condition)
                         (push (list delay condition) retries))))
      (let ((headers (http-sse-client-request-headers client)))
        (expect (test-header-value headers "accept") :to-equalp "text/event-stream")
        (expect (test-header-value headers "cache-control") :to-equalp "no-cache"))
      (start-http-sse-client-response client (test-response))
      (feed-http-sse-client client
                            (format nil "retry: 2500~%id: 9~%data: client~%~%"))
      (finish-http-sse-client-response client :eof)
      (expect opened :to-equalp 1)
      (expect (length events) :to-equalp 1)
      (expect (http-sse-event-id (first events)) :to-equalp "9")
      (expect (http-sse-client-last-event-id client) :to-equalp "9")
      (expect (http-sse-client-retry-attempt client) :to-equalp 1)
      (expect (first (first retries)) :to-equalp 2.5d0)
      (expect (first closed) :to-equalp :eof)))

  (it "builds replay-aware requests and consumes response streams"
    (let* ((client (make-http-sse-client
                    :url "http://127.0.0.1/events"
                    :initial-last-event-id "resume"
                    :retry-policy (test-retry-policy)))
           (request (make-http-sse-client-request
                     client
                     :headers (list
                               (http-message-kit:make-http-header
                                "accept" "text/event-stream, text/plain")))))
      (expect (http-message-kit:http-request-method request) :to-equalp "GET")
      (expect (http-message-kit:http-uri-string
               (http-message-kit:http-request-uri request))
              :to-equalp "http://127.0.0.1/events")
      (expect (test-header-value
               (http-message-kit:http-request-headers request) "accept")
              :to-equalp "text/event-stream, text/plain")
      (expect (test-header-value
               (http-message-kit:http-request-headers request) "last-event-id")
              :to-equalp "resume")
      (signals sse-error
        (make-http-sse-client-request client :method "POST"))
      (signals sse-error
        (make-http-sse-client-request client :method "get"))
      (signals sse-error
        (make-http-sse-client-request
         client
         :headers (list
                   (http-message-kit:make-http-header
                    "last-event-id" "1")
                   (http-message-kit:make-http-header
                    "last-event-id" "2"))))
      (setf (http-sse-client-last-event-id client) "9")
      (expect (test-header-value
               (http-message-kit:http-request-headers
                (make-http-sse-client-request client))
               "last-event-id")
              :to-equalp "9")
      (setf (http-sse-client-last-event-id client) "é-イベント")
      (let ((header-value
              (test-header-value
               (http-message-kit:http-request-headers
                (make-http-sse-client-request client))
               "last-event-id")))
        (expect (map 'list #'char-code header-value)
                :to-equalp
                (coerce (cl-codec-kit:string-to-octets
                         "é-イベント"
                         :encoding :utf-8)
                        'list)))
      (setf (http-sse-client-last-event-id client)
            (format nil "invalid~%id"))
      (signals sse-error
        (http-sse-client-request-headers client)))
    (signals sse-error
      (make-http-sse-client
       :initial-last-event-id (format nil "invalid~%id")))
    (let* ((events nil)
          (chunks (list (cl-codec-kit:string-to-octets
                         (format nil "id: 1~%"))
                        (cl-codec-kit:string-to-octets
                         (format nil "data: streamed~%~%"))
                        nil))
          (client (make-http-sse-client
                   :retry-policy (test-retry-policy)
                   :on-event (lambda (event)
                               (push event events)))))
      (consume-http-sse-client-response
       client
       (make-http-sse-response-stream
        :body-function (lambda () (pop chunks))))
      (expect (length events) :to-equalp 1)
      (unless events
        (error "Expected one streamed SSE event."))
      (expect (http-sse-event-data (first events)) :to-equalp "streamed"))
    (let ((client (make-http-sse-client
                   :retry-policy (test-retry-policy))))
      (consume-http-sse-client-response
       client (test-response :status 204 :content-type nil))
      (expect (http-sse-client-stopped-p client) :to-equalp t)))

  (it "rejects malformed direct response bodies as SSE errors"
    (let* ((client (make-http-sse-client :retry-policy (test-retry-policy)))
           (response (test-response)))
      (setf (http-message-kit::%response-body response) "not-octets")
      (signals sse-error
        (consume-http-sse-client-response client response))))

  (it "rejects empty response stream chunks without spinning"
    (let ((calls 0)
          (client (make-http-sse-client
                   :retry-policy (test-retry-policy))))
      (signals sse-error
        (consume-http-sse-client-response
         client
         (make-http-sse-response-stream
          :body-function
          (lambda ()
            (incf calls)
            (make-array 0 :element-type '(unsigned-byte 8))))))
      (expect calls :to-equalp 1)
      (expect (http-sse-client-connected-p client) :to-equalp nil)))

  (it "stops stream consumption when an event callback stops the client"
    (let ((calls 0)
          (events 0)
          (chunks (list (cl-codec-kit:string-to-octets
                         (format nil "data: stop~%~%"))
                        (cl-codec-kit:string-to-octets "unexpected")
                        nil))
          (client nil))
      (setf client
            (make-http-sse-client
             :retry-policy (test-retry-policy)
             :on-event (lambda (event)
                         (declare (ignore event))
                         (incf events)
                         (stop-http-sse-client client :callback))))
      (consume-http-sse-client-response
       client
       (make-http-sse-response-stream
        :body-function (lambda ()
                         (incf calls)
                         (pop chunks))))
      (expect events :to-equalp 1)
      (expect calls :to-equalp 1)
      (expect (http-sse-client-stopped-p client) :to-equalp t)
      (expect (http-sse-client-connected-p client) :to-equalp nil)))

  (it "rejects a response stream whose element type disagrees"
    (signals sse-error
      (read-http-sse-client-response
       (make-http-sse-client)
       (make-string-input-stream "data: wrong stream~%~%")
       :element-type :octets)))

  (it "rejects an oversized response read buffer before allocation"
    (signals sse-size-limit-exceeded
      (read-http-sse-client-response
       (make-http-sse-client)
       (make-string-input-stream "")
       :element-type :characters
       :buffer-size (1+ +http-sse-max-read-buffer-size+))))

  (it "defaults to persistent reconnects and rejects overlapping responses"
    (let ((client (make-http-sse-client)))
      (expect (http-sse-client-retry-forever-p client) :to-equalp t)
      (start-http-sse-client-response client (test-response))
      (signals sse-error
        (start-http-sse-client-response client (test-response)))
      (stop-http-sse-client client :test))
    (expect (http-sse-client-retry-forever-p
             (make-http-sse-client :retry-forever-p nil))
            :to-equalp nil))

  (it "honors Retry-After on retryable responses"
    (let* ((retry nil)
          (client (make-http-sse-client
                   :retry-policy (test-retry-policy)
                   :on-retry (lambda (delay condition)
                               (declare (ignore condition))
                               (setf retry delay)))))
      (signals sse-http-error
        (start-http-sse-client-response
         client
         (test-response
          :status 503
          :headers (list
                    (http-message-kit:make-http-header
                     "retry-after" "7")))))
      (expect retry :to-equalp 7d0)))

  (it "parses bounded Retry-After delta-seconds with leading zeroes"
    (let* ((retry nil)
           (client (make-http-sse-client
                    :retry-policy (test-retry-policy)
                    :on-retry (lambda (delay condition)
                                (declare (ignore condition))
                                (setf retry delay)))))
      (signals sse-http-error
        (start-http-sse-client-response
         client
         (test-response
          :status 503
          :headers (list
                    (http-message-kit:make-http-header
                     "retry-after" "000000007")))))
      (expect retry :to-equalp 7d0)))

  (it "honors HTTP-date Retry-After values within the policy cap"
    (let* ((retry nil)
          (client (make-http-sse-client
                   :retry-policy (test-retry-policy :max-attempts 1)
                   :on-retry (lambda (delay condition)
                               (declare (ignore condition))
                               (setf retry delay)))))
      (signals sse-http-error
        (start-http-sse-client-response
         client
         (test-response
          :status 503
          :headers (list
                    (http-message-kit:make-http-header
                     "retry-after"
                     (test-http-date (+ (get-universal-time) 30)))))))
      (expect retry :to-equalp 10d0)))

  (it "ignores ambiguous duplicate Retry-After values"
    (let* ((retry nil)
          (client (make-http-sse-client
                   :retry-policy (test-retry-policy)
                   :on-retry (lambda (delay condition)
                               (declare (ignore condition))
                               (setf retry delay)))))
      (signals sse-http-error
        (start-http-sse-client-response
         client
         (test-response
          :status 503
          :headers (list
                    (http-message-kit:make-http-header
                     "retry-after" "7")
                    (http-message-kit:make-http-header
                     "retry-after" "8")))))
      (expect retry :to-equalp 2d0)))

  (it "surfaces redirects and cancels an active transport once"
    (let ((redirect nil)
          (cancelled 0)
          (closed nil)
          (client nil))
      (setf client
            (make-http-sse-client
             :retry-policy (test-retry-policy)
             :on-redirect
             (lambda (location response)
               (setf redirect
                     (list location
                           (http-message-kit:http-response-status response))))
             :cancel-transport
             (lambda (client reason)
               (declare (ignore client reason))
               (incf cancelled))))
      (signals sse-http-error
        (start-http-sse-client-response
         client
         (test-response
          :status 307
          :content-type nil
          :headers (list
                    (http-message-kit:make-http-header
                     "location" "https://example.test/next")))))
      (expect redirect :to-equalp '("https://example.test/next" 307))
      (expect cancelled :to-equalp 1)
      (setf redirect nil)
      (signals sse-http-error
        (start-http-sse-client-response
         (make-http-sse-client :retry-policy (test-retry-policy))
         (test-response
          :status 307
          :content-type nil
          :headers (list
                    (http-message-kit:make-http-header
                     "location" "https://example.test/one")
                    (http-message-kit:make-http-header
                     "location" "https://example.test/two")))))
      (expect redirect :to-equalp nil)
      (setf closed nil
            client
            (make-http-sse-client
             :cancel-transport
             (lambda (client reason)
               (declare (ignore client reason))
               (incf cancelled))
             :on-close (lambda (reason) (push reason closed))))
      (start-http-sse-client-response client (test-response))
      (stop-http-sse-client client :manual)
      (stop-http-sse-client client :again)
      (expect cancelled :to-equalp 2)
      (expect (http-sse-client-transport-cancelled-p client)
              :to-equalp t)
      (expect closed :to-equalp '(:manual))))

  (it "reports response body failures through the retry boundary"
    (let* ((errors nil)
          (cancelled 0)
          (client
            (make-http-sse-client
             :retry-policy (test-retry-policy)
             :on-error (lambda (condition) (push condition errors))
             :cancel-transport
             (lambda (client reason)
               (declare (ignore client reason))
               (incf cancelled)))))
      (signals error
        (consume-http-sse-client-response
         client
         (make-http-sse-response-stream
          :body-function (lambda () (error "body failure")))))
      (expect (length errors) :to-equalp 1)
      (expect cancelled :to-equalp 1)
      (expect (http-sse-client-transport-cancelled-p client)
              :to-equalp t)))

  (it "does not duplicate close notification when an error callback stops the client"
    (let ((closed nil)
          (client nil))
      (setf client
            (make-http-sse-client
             :retry-policy (test-retry-policy :max-attempts 2)
             :max-line-bytes 1
             :on-error (lambda (condition)
                         (declare (ignore condition))
                         (stop-http-sse-client client :callback-stop))
             :on-close (lambda (reason)
                         (push reason closed))))
      (start-http-sse-client-response client (test-response))
      (signals sse-size-limit-exceeded
        (feed-http-sse-client client "data: too long~%"))
      (expect (length closed) :to-equalp 1)
      (expect (first closed) :to-equalp :callback-stop)
      (expect (http-sse-client-stopped-p client) :to-equalp t)))

  (it "does not report EOF after an event callback stops during finish"
    (let ((events 0)
          (errors nil)
          (closed nil)
          (client nil))
      (setf client
            (make-http-sse-client
             :retry-policy (test-retry-policy :max-attempts 2)
             :on-event (lambda (event)
                         (declare (ignore event))
                         (incf events)
                         (stop-http-sse-client client :callback-stop))
             :on-error (lambda (condition)
                         (push condition errors))
             :on-close (lambda (reason)
                         (push reason closed))))
      (start-http-sse-client-response client (test-response))
      (feed-http-sse-client
       client
       (format nil "data: final~C~C~C" #\Return #\Linefeed #\Return))
      (expect events :to-equalp 0)
      (finish-http-sse-client-response client :eof)
      (expect events :to-equalp 1)
      (expect errors :to-equalp nil)
      (expect closed :to-equalp '(:callback-stop))
      (expect (http-sse-client-stopped-p client) :to-equalp t)))

  (it "does not acknowledge an event when its callback fails"
    (let ((client
            (make-http-sse-client
             :retry-policy (test-retry-policy :max-attempts 1)
             :on-event (lambda (event)
                         (declare (ignore event))
                         (error "event callback failure")))))
      (start-http-sse-client-response client (test-response))
      (signals error
        (feed-http-sse-client client
                              (format nil
                                      "retry: 2500~%id: lost~%data: event~%~%")))
      (expect (http-sse-client-last-event-id client) :to-equalp "")
      (expect (http-sse-client-retry-delay-override client)
              :to-equalp 2.5d0)))

  (it "isolates lifecycle callback failures from response conditions"
    (let ((client
            (make-http-sse-client
             :retry-policy (test-retry-policy :max-attempts 1)
             :on-error (lambda (condition)
                         (declare (ignore condition))
                         (error "on-error callback failure"))
             :on-retry (lambda (delay condition)
                         (declare (ignore delay condition))
                         (error "on-retry callback failure"))
             :on-close (lambda (reason)
                         (declare (ignore reason))
                         (error "on-close callback failure"))))
          (condition nil))
      (handler-case
          (start-http-sse-client-response client (test-response :status 503))
        (sse-http-error (caught)
          (setf condition caught)))
      (expect (typep condition 'sse-http-error) :to-equalp t)
      (expect (typep (http-sse-client-callback-error client) 'error)
              :to-equalp t))
    (let ((client
            (make-http-sse-client
             :on-open (lambda (response)
                        (declare (ignore response))
                        (error "on-open callback failure")))))
      (start-http-sse-client-response client (test-response))
      (expect (http-sse-client-connected-p client) :to-equalp t)
      (expect (typep (http-sse-client-callback-error client) 'error)
              :to-equalp t)
      (stop-http-sse-client client :test)))

  (it "rejects duplicate response content types at the client boundary"
    (let ((client (make-http-sse-client :retry-policy (test-retry-policy))))
      (signals sse-http-error
        (start-http-sse-client-response
         client
         (test-response
          :content-type nil
          :headers (list
                    (http-message-kit:make-http-header
                     "content-type" "text/event-stream")
                    (http-message-kit:make-http-header
                     "content-type" "text/event-stream")))))))

  (it "handles terminal and retryable HTTP responses"
    (let* ((closed nil)
          (terminal (make-http-sse-client
                     :on-close (lambda (reason) (push reason closed)))))
      (start-http-sse-client-response
       terminal
       (test-response :status 204 :content-type nil))
      (expect (http-sse-client-stopped-p terminal) :to-equalp t)
      (expect (first closed) :to-equalp :no-content))
    (let* ((errors nil)
          (retries nil)
          (closed nil)
          (retryable (make-http-sse-client
                      :retry-policy (test-retry-policy :max-attempts 2)
                      :on-error (lambda (condition) (push condition errors))
                      :on-retry (lambda (delay condition)
                                  (push (list delay condition) retries))
                      :on-close (lambda (reason) (push reason closed)))))
      (signals sse-http-error
        (start-http-sse-client-response retryable (test-response :status 500)))
      (expect (length errors) :to-equalp 1)
      (expect (typep (first errors) 'sse-http-error) :to-equalp t)
      (expect (length retries) :to-equalp 1)
      (expect (length closed) :to-equalp 1)
      (expect (typep (first closed) 'sse-http-error) :to-equalp t)
      (expect (http-sse-client-retry-attempt retryable) :to-equalp 1)))

  (it "supports client CPS and scoped cleanup"
    (let ((client (make-http-sse-client
                   :retry-policy (test-retry-policy)))
          (success nil)
          (failure nil))
      (start-http-sse-client-response/k
       client
       (test-response)
       (lambda (value) (setf success value))
       (lambda (condition) (setf failure condition)))
      (expect success :to-equalp client)
      (expect failure :to-equalp nil)
      (stop-http-sse-client/k
       client
       (lambda (value) (setf success value))
       (lambda (condition) (setf failure condition))
       :test)
      (expect success :to-equalp client)
      (expect failure :to-equalp nil))
    (let ((close-reason nil))
      (with-http-sse-client (client
                              :retry-policy (test-retry-policy)
                              :on-close (lambda (reason)
                                          (setf close-reason reason)))
        (start-http-sse-client-response client (test-response)))
      (expect close-reason :to-equalp :scope-exit))))
