(in-package #:sse-kit)

(defconstant +http-sse-client-connecting+ 0)
(defconstant +http-sse-client-open+ 1)
(defconstant +http-sse-client-closed+ 2)
(defconstant +http-sse-client-max-server-delay-seconds+ 31536000d0)
(defconstant +http-sse-max-read-buffer-size+ (* 1024 1024))

(defstruct (http-sse-client
             (:constructor %make-http-sse-client
                 (&key url on-event on-open on-error on-close on-retry
                        on-redirect retry-policy parser-limits retry-forever-p
                        cancel-transport last-event-id)))
  url
  on-event
  on-open
  on-error
  on-close
  on-retry
  on-redirect
  cancel-transport
  retry-policy
  parser-limits
  retry-forever-p
  (last-event-id "")
  (connected-p nil)
  (stopped-p nil)
  (ready-state +http-sse-client-connecting+)
  (retry-attempt 0)
  retry-delay-override
  retry-after-delay
  next-retry-delay
  response
  parser
  (transport-cancelled-p nil)
  transport-cancel-error
  (close-notified-p nil)
  callback-error)

(defun %sse-client-check-callback (value name)
  (when (and value (not (functionp value)))
    (%sse-protocol-error
     (format nil "~A must be a function or NIL." name)
     value))
  value)

(defun %sse-client-check-url (value)
  (when (and value
             (not (or (stringp value)
                      (http-message-kit:http-uri-p value))))
    (%sse-protocol-error
     "URL must be a string, HTTP URI, or NIL."
     value))
  value)

(defun %sse-client-check-last-event-id (value)
  (unless (and (stringp value)
               (%sse-no-line-breaks-p value)
               (not (find #\Null value :test #'char=)))
    (%sse-protocol-error
     "LAST-EVENT-ID must be a string without line breaks or NUL."
     value))
  value)

(defun %sse-client-default-retry-policy ()
  (resilience-kit:make-retry-policy
   :max-attempts 8
   :initial-delay 3d0
   :multiplier 2d0
   :max-delay 300d0
   :jitter :full
   :retry-safe-p t
   :condition-classifier
   (lambda (condition attempt)
     (declare (ignore attempt))
     (cond ((typep condition 'sse-client-disconnected) t)
           ((typep condition 'sse-http-error)
            (let ((status (sse-http-error-status condition)))
              (or (= status 408)
                  (= status 429)
                  (<= 500 status 599)
                  (member status '(301 302 303 307 308)))))
           ((typep condition 'sse-error) nil)
           (t t)))))

(defun %sse-client-check-retry-policy (value)
  (unless (typep value 'resilience-kit:retry-policy)
    (%sse-protocol-error
     "RETRY-POLICY must be a RESILIENCE-KIT:RETRY-POLICY value."
     value))
  value)

(defun make-http-sse-client
    (&key url on-event on-open on-error on-close on-retry on-redirect
          retry-policy (retry-forever-p t) cancel-transport
          (initial-last-event-id "")
          (max-events +http-sse-default-max-events+)
          (max-line-bytes +http-sse-default-max-line-bytes+)
          (max-data-bytes +http-sse-default-max-data-bytes+)
          (max-comments +http-sse-default-max-comments+)
          (max-comment-bytes +http-sse-default-max-comment-bytes+)
          (max-input-bytes +http-sse-default-max-input-bytes+))
  "Create an EventSource state machine around a retry policy.

The caller owns the HTTP transport, response body reader, timer, and socket.
ON-EVENT receives HTTP-SSE-EVENT values. INITIAL-LAST-EVENT-ID seeds the
replay cursor used in the first request. ON-OPEN receives an HTTP response,
ON-ERROR receives a condition, ON-CLOSE receives a reason, ON-RETRY receives
the computed delay and condition, and ON-REDIRECT receives a Location value
and response. CANCEL-TRANSPORT receives CLIENT and a reason when STOP is
called or an active response is being abandoned for retry. Conditions from
lifecycle callbacks are retained in HTTP-SSE-CLIENT-CALLBACK-ERROR; ON-EVENT
conditions propagate through the body-processing operation."
  (%sse-client-check-url url)
  (%sse-client-check-callback on-event "ON-EVENT")
  (%sse-client-check-callback on-open "ON-OPEN")
  (%sse-client-check-callback on-error "ON-ERROR")
  (%sse-client-check-callback on-close "ON-CLOSE")
  (%sse-client-check-callback on-retry "ON-RETRY")
  (%sse-client-check-callback on-redirect "ON-REDIRECT")
  (%sse-client-check-callback cancel-transport "CANCEL-TRANSPORT")
  (%sse-client-check-last-event-id initial-last-event-id)
  (let ((policy (%sse-client-check-retry-policy
                 (or retry-policy (%sse-client-default-retry-policy)))))
    (%make-http-sse-client
     :url url
     :on-event on-event
     :on-open on-open
     :on-error on-error
     :on-close on-close
     :on-retry on-retry
     :on-redirect on-redirect
     :cancel-transport cancel-transport
     :last-event-id initial-last-event-id
     :retry-policy policy
     :retry-forever-p (not (null retry-forever-p))
     :parser-limits (list :max-events max-events
                          :max-line-bytes max-line-bytes
                          :max-data-bytes max-data-bytes
                          :max-comments max-comments
                          :max-comment-bytes max-comment-bytes
                          :max-input-bytes max-input-bytes))))

(defun http-sse-client-request-headers (client)
  "Return default headers for the next SSE request."
  (unless (http-sse-client-p client)
    (%sse-protocol-error "Expected an HTTP-SSE-CLIENT value." client))
  (let ((last-event-id
          (%sse-client-check-last-event-id
           (http-sse-client-last-event-id client)))
        (headers (list (http-message-kit:make-http-header
                        "accept" "text/event-stream")
                       (http-message-kit:make-http-header
                        "cache-control" "no-cache"))))
    (if (plusp (length last-event-id))
        (append headers
                (list (http-message-kit:make-http-header
                       "last-event-id"
                       (%sse-http-header-value-from-utf8
                        last-event-id))))
        headers)))

(defun %sse-client-header-name (header)
  (string-downcase (http-message-kit:http-header-name header)))

(defun %sse-client-merge-headers (defaults explicit)
  (let ((explicit-names (mapcar #'%sse-client-header-name explicit)))
    (when (> (count "last-event-id" explicit-names :test #'string=) 1)
      (%sse-protocol-error
       "An SSE client request may contain at most one Last-Event-ID header."
       explicit))
    (append explicit
            (remove-if
             (lambda (header)
               (member (%sse-client-header-name header)
                       explicit-names :test #'string=))
             defaults))))

(defun make-http-sse-client-request
    (client &key uri (method "GET") request-target headers
                  (protocol-version "HTTP/1.1"))
  "Build an HTTP GET request with SSE negotiation and replay headers.

URI overrides the URL supplied when CLIENT was created. Explicit HEADERS
replace default headers with the same field name. The resulting request is
ready for an HTTP transport to send."
  (unless (http-sse-client-p client)
    (%sse-protocol-error "Expected an HTTP-SSE-CLIENT value." client))
  (unless (and (stringp method) (string= method "GET"))
    (%sse-protocol-error
     "HTTP SSE client requests must use GET."
     method))
  (let ((target (or uri (http-sse-client-url client))))
    (unless target
      (%sse-protocol-error
       "An SSE client request requires URI or a client URL."
       client))
    (http-message-kit:make-http-request
     :method method
     :uri target
     :request-target request-target
     :headers (%sse-client-merge-headers
               (http-sse-client-request-headers client)
               (or headers nil))
     :protocol-version protocol-version)))

(defun %sse-client-response-p (response)
  (or (http-message-kit:http-response-p response)
      (http-message-kit:http-response-stream-p response)))

(defun %sse-client-response-status (response)
  (cond ((http-message-kit:http-response-p response)
         (http-message-kit:http-response-status response))
        ((http-message-kit:http-response-stream-p response)
         (http-message-kit:http-response-stream-status response))
        (t
         (%sse-protocol-error
          "Expected an HTTP response or response stream."
          response))))

(defun %sse-client-response-headers (response)
  (cond ((http-message-kit:http-response-p response)
         (http-message-kit:http-response-headers response))
        ((http-message-kit:http-response-stream-p response)
         (http-message-kit:http-response-stream-headers response))
        (t
         (%sse-protocol-error
          "Expected an HTTP response or response stream."
          response))))

(defun %sse-client-response-body (response)
  (when (http-message-kit:http-response-p response)
    (http-message-kit:http-response-body response)))

(defun %sse-client-response-body-function (response)
  (when (http-message-kit:http-response-stream-p response)
    (http-message-kit:http-response-stream-body-function response)))

(defun %sse-client-http-error (response message &optional detail)
  (let ((status (%sse-client-response-status response)))
    (make-condition 'sse-http-error
                    :message message
                    :operation :http-response
                    :detail (append (list :status status) detail)
                    :status status
                    :headers (%sse-client-response-headers response))))

(defun %sse-client-call-callback (client callback &rest arguments)
  (when callback
    (handler-case
        (apply callback arguments)
      (condition (condition)
        (setf (http-sse-client-callback-error client) condition))))
  client)

(defun %sse-client-notify-close-reason (client reason)
  (unless (http-sse-client-close-notified-p client)
    (setf (http-sse-client-close-notified-p client) t)
    (%sse-client-call-callback client
                               (http-sse-client-on-close client)
                               reason))
  client)

(defun %sse-client-report-error (client condition)
  (%sse-client-call-callback client
                             (http-sse-client-on-error client)
                             condition)
  condition)

(defun %sse-client-cancel-active-transport (client reason)
  (when (and (http-sse-client-cancel-transport client)
             (not (http-sse-client-transport-cancelled-p client)))
    (setf (http-sse-client-transport-cancelled-p client) t)
    (handler-case
        (funcall (http-sse-client-cancel-transport client) client reason)
      (condition (condition)
        (setf (http-sse-client-transport-cancel-error client) condition))))
  client)

(defun %sse-client-ascii-digit-p (character)
  (and (char>= character #\0)
       (char<= character #\9)))

(defun %sse-client-delay-limit-seconds (maximum)
  (cond ((null maximum)
         +http-sse-client-max-server-delay-seconds+)
        ((<= maximum 0)
         0d0)
        ((>= maximum +http-sse-client-max-server-delay-seconds+)
         +http-sse-client-max-server-delay-seconds+)
        (t
         (coerce maximum 'double-float))))

(defun %sse-client-bound-delay (delay maximum)
  (let ((limit (%sse-client-delay-limit-seconds maximum)))
    (min limit (max 0d0 (coerce delay 'double-float)))))

(defun %sse-client-parse-bounded-decimal-seconds (value maximum)
  (let* ((limit (%sse-client-delay-limit-seconds maximum))
         (limit-integer (ceiling limit))
         (trimmed (string-trim '(#\Space #\Tab) value)))
    (when (and (plusp (length trimmed))
               (every #'%sse-client-ascii-digit-p trimmed))
      (let* ((without-leading-zeroes (string-left-trim "0" trimmed))
             (normalized (if (plusp (length without-leading-zeroes))
                             without-leading-zeroes
                             "0")))
        (if (> (length normalized) (length (princ-to-string limit-integer)))
            limit
            (let ((seconds (parse-integer normalized)))
              (if (>= seconds limit-integer)
                  limit
                  (coerce seconds 'double-float))))))))

(defun %sse-client-parse-delta-seconds (value &optional maximum)
  (%sse-client-parse-bounded-decimal-seconds value maximum))

(defun %sse-client-milliseconds-to-seconds (milliseconds maximum)
  (let* ((limit (%sse-client-delay-limit-seconds maximum))
         (limit-milliseconds (ceiling (* limit 1000d0))))
    (if (>= milliseconds limit-milliseconds)
        limit
        (/ (coerce (max 0 milliseconds) 'double-float) 1000d0))))

(defun %sse-client-parse-http-time (value &optional maximum)
  (when (stringp value)
    (let ((text (string-trim '(#\Space #\Tab) value)))
      (labels ((split (text separator)
                 (let ((fields nil)
                       (start 0)
                       (length (length text)))
                   (loop for index from 0 to length
                         do (when (or (= index length)
                                      (char= (char text index) separator))
                              (unless (= start index)
                                (push (subseq text start index) fields))
                              (setf start (1+ index))))
                   (nreverse fields)))
               (digits-only-p (text)
                 (and (stringp text)
                      (plusp (length text))
                      (every #'%sse-client-ascii-digit-p text)))
               (number (text minimum-length maximum-length)
                 (and (digits-only-p text)
                      (<= minimum-length (length text) maximum-length)
                      (parse-integer text)))
               (weekday-number (name)
                 (loop for short in '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
                       for full in '("Sunday" "Monday" "Tuesday" "Wednesday"
                                     "Thursday" "Friday" "Saturday")
                       for index from 0
                       when (or (string-equal name short)
                                (string-equal name full))
                         return index))
               (month-number (name)
                 (let ((position
                         (position name
                                   '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                   :test #'string-equal)))
                   (and position (1+ position))))
               (clock (text)
                 (when (and (stringp text)
                            (= (length text) 8)
                            (char= (char text 2) #\:)
                            (char= (char text 5) #\:)
                            (digits-only-p (subseq text 0 2))
                            (digits-only-p (subseq text 3 5))
                            (digits-only-p (subseq text 6 8)))
                   (values (parse-integer text :end 2)
                           (parse-integer text :start 3 :end 5)
                           (parse-integer text :start 6))))
               (finish (weekday day month year hour minute second maximum)
                 (when (and weekday day month year hour minute second
                            (plusp year)
                            (<= 1 day 31)
                            (<= 1 month 12)
                            (<= 0 hour 23)
                            (<= 0 minute 59)
                            (<= 0 second 59))
                   (handler-case
                       (let ((timestamp
                               (encode-universal-time second minute hour day
                                                      month year 0)))
                         (multiple-value-bind
                               (decoded-second decoded-minute decoded-hour
                                decoded-day decoded-month decoded-year
                                decoded-weekday)
                             (decode-universal-time timestamp 0)
                           (when (and (= decoded-second second)
                                      (= decoded-minute minute)
                                      (= decoded-hour hour)
                                      (= decoded-day day)
                                      (= decoded-month month)
                                      (= decoded-year year)
                                      (= decoded-weekday weekday))
                            (%sse-client-bound-delay
                             (- timestamp (get-universal-time))
                             maximum))))
                     (error () nil)))))
        (let ((fields (split text #\Space)))
          (cond
            ((and (= (length fields) 6)
                  (string-equal (sixth fields) "GMT"))
             (let* ((weekday-name (string-right-trim "," (first fields)))
                    (weekday (weekday-number weekday-name))
                    (day (number (second fields) 2 2))
                    (month (month-number (third fields)))
                    (year (number (fourth fields) 4 4)))
               (multiple-value-bind (hour minute second)
                   (clock (fifth fields))
                 (finish weekday day month year hour minute second maximum))))
            ((and (= (length fields) 4)
                  (string-equal (fourth fields) "GMT"))
             (let* ((weekday-name (string-right-trim "," (first fields)))
                    (weekday (weekday-number weekday-name))
                    (date-fields (split (second fields) #\-))
                    (day (and (= (length date-fields) 3)
                              (number (first date-fields) 2 2)))
                    (month (and (= (length date-fields) 3)
                                (month-number (second date-fields))))
                    (short-year (and (= (length date-fields) 3)
                                     (number (third date-fields) 2 2)))
                    (year (and short-year
                               (if (< short-year 50)
                                   (+ 2000 short-year)
                                   (+ 1900 short-year)))))
               (multiple-value-bind (hour minute second)
                   (clock (third fields))
                 (finish weekday day month year hour minute second maximum))))
            ((= (length fields) 5)
             (let* ((weekday (weekday-number (first fields)))
                    (month (month-number (second fields)))
                    (day (number (third fields) 1 2))
                    (year (number (fifth fields) 4 4)))
               (multiple-value-bind (hour minute second)
                   (clock (fourth fields))
                 (finish weekday day month year hour minute second maximum))))))))))

(defun %sse-client-retry-after-delay (response &optional maximum)
  (let ((value (%sse-http-single-header-value
                (%sse-client-response-headers response)
                "retry-after")))
    (when value
      (or (%sse-client-parse-delta-seconds value maximum)
          (%sse-client-parse-http-time value maximum)))))

(defun %sse-client-delay (client attempt decision)
  (let* ((policy (http-sse-client-retry-policy client))
         (computed (resilience-kit:compute-backoff-delay policy attempt))
         (hint (resilience-kit:retry-decision-delay-hint decision))
         (server-delay (http-sse-client-retry-delay-override client))
         (retry-after (http-sse-client-retry-after-delay client))
         (delay (max computed
                     (or hint 0d0)
                     (or server-delay 0d0)
                     (or retry-after 0d0)))
         (maximum (%sse-client-delay-limit-seconds
                   (resilience-kit:retry-policy-max-delay policy))))
    (min maximum (max 0d0 (coerce delay 'double-float)))))

(defun %sse-client-schedule-retry (client condition)
  (%sse-client-cancel-active-transport client condition)
  (let* ((attempt (1+ (http-sse-client-retry-attempt client)))
         (policy (http-sse-client-retry-policy client)))
    (setf (http-sse-client-retry-attempt client) attempt
          (http-sse-client-connected-p client) nil
          (http-sse-client-parser client) nil
          (http-sse-client-response client) nil
          (http-sse-client-ready-state client)
          +http-sse-client-connecting+)
    (if (http-sse-client-stopped-p client)
        (progn
          (setf (http-sse-client-next-retry-delay client) nil
                (http-sse-client-ready-state client)
                +http-sse-client-closed+)
          nil)
        (multiple-value-bind (retry-p decision)
            (resilience-kit:retry-policy-should-retry-p
             policy attempt :condition condition)
          (when (and (not retry-p)
                     (http-sse-client-retry-forever-p client)
                     (resilience-kit:retry-policy-retry-safe-p policy)
                     (resilience-kit:retry-decision-retry-p decision))
            (setf retry-p t))
          (if retry-p
              (let ((delay (%sse-client-delay client attempt decision)))
                (setf (http-sse-client-next-retry-delay client) delay
                      (http-sse-client-retry-after-delay client) nil)
                (%sse-client-call-callback
                 client
                 (http-sse-client-on-retry client)
                 delay condition)
                t)
              (progn
                (setf (http-sse-client-next-retry-delay client) nil
                      (http-sse-client-stopped-p client) t
                      (http-sse-client-connected-p client) nil
                      (http-sse-client-parser client) nil
                      (http-sse-client-ready-state client)
                      +http-sse-client-closed+)
                nil))))))

(defun %sse-client-notify-close (client reason retry-p)
  (%sse-client-notify-close-reason
   client
   (if retry-p (or reason :eof) :retry-exhausted)))

(defun %sse-client-fail-response (client condition &optional reason)
  (%sse-client-report-error client condition)
  (let ((retry-p (%sse-client-schedule-retry client condition)))
    (%sse-client-notify-close client (or reason condition) retry-p)
    retry-p))

(defun %sse-client-sync-parser-state (client &key (event-id-p t))
  (let ((parser (http-sse-client-parser client)))
    (when parser
      (when event-id-p
        (setf (http-sse-client-last-event-id client)
              (http-sse-parser-last-event-id parser)))
      (when (http-sse-parser-retry parser)
        (setf (http-sse-client-retry-delay-override client)
              (%sse-client-milliseconds-to-seconds
               (http-sse-parser-retry parser)
               (resilience-kit:retry-policy-max-delay
                (http-sse-client-retry-policy client))))))
  client))

(defun %sse-client-event-callback (client)
  (lambda (event)
    (%sse-client-sync-parser-state client :event-id-p nil)
    (when (http-sse-client-on-event client)
      (funcall (http-sse-client-on-event client) event))
    (%sse-client-sync-parser-state client)))

(defun %sse-client-disconnected-condition (reason)
  (make-condition 'sse-client-disconnected
                  :message "The SSE response ended before the client was stopped."
                  :operation :response-finished
                  :detail reason))

(defun %sse-client-start-parser (client)
  (setf (http-sse-client-parser client)
        (apply #'make-http-sse-parser
               :initial-last-event-id
               (http-sse-client-last-event-id client)
               :on-event (%sse-client-event-callback client)
               (http-sse-client-parser-limits client))
        (http-sse-client-connected-p client) t
        (http-sse-client-ready-state client) +http-sse-client-open+
        (http-sse-client-retry-attempt client) 0
        (http-sse-client-next-retry-delay client) nil
        (http-sse-client-transport-cancelled-p client) nil)
  client)

(defun %sse-client-redirect-status-p (status)
  (member status '(301 302 303 307 308) :test #'=))

(defun start-http-sse-client-response (client response)
  "Validate RESPONSE and start parsing its HTTP response body.

A 204 response permanently stops the client. A successful response must have
status 200 and a valid text/event-stream content type. Redirect responses are
reported to ON-REDIRECT and remain transport decisions for the caller."
  (unless (http-sse-client-p client)
    (%sse-protocol-error "Expected an HTTP-SSE-CLIENT value." client))
  (unless (%sse-client-response-p response)
    (%sse-protocol-error
     "Expected an HTTP-MESSAGE-KIT response or response stream value."
     response))
  (when (http-sse-client-stopped-p client)
    (%sse-protocol-error "An HTTP-SSE-CLIENT has been stopped." client))
  (when (http-sse-client-connected-p client)
    (%sse-protocol-error
     "An HTTP-SSE-CLIENT already has an active response."
     client))
  (setf (http-sse-client-transport-cancelled-p client) nil
        (http-sse-client-transport-cancel-error client) nil
        (http-sse-client-callback-error client) nil
        (http-sse-client-close-notified-p client) nil)
  (let* ((status (%sse-client-response-status response))
         (policy (http-sse-client-retry-policy client))
         (retry-after
           (%sse-client-retry-after-delay
            response
            (resilience-kit:retry-policy-max-delay policy)))
         (headers (%sse-client-response-headers response)))
    (setf (http-sse-client-response client) response
          (http-sse-client-retry-after-delay client) retry-after)
    (cond ((= status 204)
           (setf (http-sse-client-stopped-p client) t
                 (http-sse-client-connected-p client) nil
                 (http-sse-client-ready-state client)
                 +http-sse-client-closed+
                 (http-sse-client-parser client) nil
                 (http-sse-client-next-retry-delay client) nil)
           (%sse-client-notify-close-reason client :no-content)
           client)
          ((/= status 200)
           (let ((location (%sse-http-single-header-value
                            headers "location"))
                 (condition
                   (%sse-client-http-error
                    response
                    "An SSE response must have HTTP status 200."
                    (list :retry-after retry-after))))
             (when (and (%sse-client-redirect-status-p status)
                        location
                        (http-sse-client-on-redirect client))
               (%sse-client-call-callback
                client
                (http-sse-client-on-redirect client)
                location response))
             (%sse-client-fail-response client condition condition)
             (error condition)))
          ((not (http-sse-content-type-valid-p
                 (%sse-http-single-header-value headers "content-type")))
           (let ((condition
                   (%sse-client-http-error
                    response
                    "An SSE response must have a valid text/event-stream content type."
                    (list :retry-after retry-after))))
             (%sse-client-fail-response client condition condition)
             (error condition)))
          (t
           (%sse-client-start-parser client)
           (%sse-client-call-callback
            client
            (http-sse-client-on-open client)
            response)
           client))))

(defun feed-http-sse-client (client input &key (start 0) end)
  "Feed one body chunk to an active client and return CLIENT."
  (unless (http-sse-client-p client)
    (%sse-protocol-error "Expected an HTTP-SSE-CLIENT value." client))
  (unless (http-sse-client-connected-p client)
    (%sse-protocol-error "An HTTP-SSE-CLIENT has no active response." client))
  (handler-case
      (progn
        (feed-http-sse-parser (http-sse-client-parser client) input
                              :start start :end end)
        (%sse-client-sync-parser-state client))
    (error (condition)
      (%sse-client-fail-response client condition condition)
      (error condition))))

(defun finish-http-sse-client-response (client &optional reason)
  "Finish an active response using strict SSE EOF semantics and schedule retry."
  (unless (http-sse-client-p client)
    (%sse-protocol-error "Expected an HTTP-SSE-CLIENT value." client))
  (unless (http-sse-client-connected-p client)
    (%sse-protocol-error "An HTTP-SSE-CLIENT has no active response." client))
  (handler-case
      (progn
        (finish-http-sse-parser (http-sse-client-parser client))
        (%sse-client-sync-parser-state client))
    (error (condition)
      (%sse-client-fail-response client condition condition)
      (error condition)))
  (when (http-sse-client-connected-p client)
    (let ((condition (%sse-client-disconnected-condition (or reason :eof))))
      (%sse-client-fail-response client condition (or reason :eof))))
  client)

(defun read-http-sse-client-response
    (client stream &key (buffer-size 4096) (element-type :octets) reason)
  "Read STREAM into CLIENT until EOF.

ELEMENT-TYPE is :OCTETS for a binary stream or :CHARACTERS for a character
stream. BUFFER-SIZE must not exceed +HTTP-SSE-MAX-READ-BUFFER-SIZE+. The
response is finished with strict SSE EOF semantics."
  (unless (streamp stream)
    (%sse-protocol-error "SSE client response input must be a stream." stream))
  (unless (and (integerp buffer-size) (plusp buffer-size))
    (%sse-protocol-error
     "BUFFER-SIZE must be a positive integer."
     buffer-size))
  (when (> buffer-size +http-sse-max-read-buffer-size+)
    (%sse-size-error
     "SSE client BUFFER-SIZE exceeds the configured maximum."
     +http-sse-max-read-buffer-size+
     buffer-size
     :read-buffer))
  (unless (member element-type '(:octets :characters) :test #'eq)
    (%sse-protocol-error
     "ELEMENT-TYPE must be :OCTETS or :CHARACTERS."
     element-type))
  (unless (if (eq element-type :octets)
              (%sse-binary-stream-p stream)
              (%sse-character-stream-p stream))
    (%sse-protocol-error
     "ELEMENT-TYPE does not match the response stream's element type."
     (list :element-type element-type
           :stream-element-type (stream-element-type stream))))
  (let ((buffer (if (eq element-type :octets)
                    (make-array buffer-size :element-type '(unsigned-byte 8))
                    (make-string buffer-size))))
    (loop
      (let ((count
              (handler-case
                  (read-sequence buffer stream)
                (condition (condition)
                  (%sse-client-response-body-error client condition)))))
        (unless (plusp count)
          (return))
        (feed-http-sse-client client buffer :end count)
        (unless (http-sse-client-connected-p client)
          (return)))))
  (when (http-sse-client-connected-p client)
    (finish-http-sse-client-response client reason)))

(defun %sse-client-response-body-error (client condition)
  (%sse-client-fail-response client condition condition)
  (error condition))

(defun consume-http-sse-client-response (client response &optional reason)
  "Consume a complete HTTP response or response stream through CLIENT.

The response stream body function is called until it returns NIL. A body
function error, an empty chunk, or a non-octet chunk is reported and treated
as a transport failure; the condition is then re-signaled to the caller."
  (start-http-sse-client-response client response)
  (when (= (%sse-client-response-status response) 204)
    (return-from consume-http-sse-client-response client))
  (unless (http-sse-client-connected-p client)
    (return-from consume-http-sse-client-response client))
  (cond
    ((http-message-kit:http-response-p response)
     (let ((body
             (handler-case
                 (%sse-client-response-body response)
               (condition (condition)
                 (%sse-client-response-body-error
                  client
                  (make-condition
                   'sse-error
                   :message "An SSE response body could not be read."
                   :operation :sse-client-response-body
                   :detail condition))))))
       (unless (or (null body) (%sse-octet-vector-p body))
         (%sse-client-response-body-error
          client
          (make-condition
           'sse-error
           :message "An SSE response body must be an octet vector."
           :operation :sse-client-response-body
           :detail body)))
       (when (and body (plusp (length body)))
         (feed-http-sse-client client body))))
    ((http-message-kit:http-response-stream-p response)
     (let ((body-function (%sse-client-response-body-function response)))
       (unless (functionp body-function)
         (%sse-client-response-body-error
          client
          (make-condition
           'sse-error
           :message "HTTP response stream body-function must be callable."
           :operation :response-body
           :detail body-function)))
       (loop
         (let ((chunk
                 (handler-case
                     (funcall body-function)
                   (error (condition)
                     (%sse-client-response-body-error client condition)))))
           (unless (http-sse-client-connected-p client)
             (return))
           (when (null chunk)
             (return))
           (unless (%sse-octet-vector-p chunk)
             (%sse-client-response-body-error
              client
              (make-condition
               'sse-error
               :message "HTTP response stream body functions must return octet vectors or NIL."
               :operation :response-body
               :detail chunk)))
           (unless (plusp (length chunk))
             (%sse-client-response-body-error
              client
              (make-condition
               'sse-error
               :message "HTTP response stream body functions must return non-empty octet vectors or NIL."
               :operation :response-body
               :detail chunk)))
           (feed-http-sse-client client chunk)
           (unless (http-sse-client-connected-p client)
             (return))))))
    (t
     (%sse-protocol-error
      "Expected an HTTP response or response stream."
      response)))
  (when (http-sse-client-connected-p client)
    (finish-http-sse-client-response client reason)))

(defun stop-http-sse-client (client &optional reason)
  "Permanently stop CLIENT and suppress further reconnect attempts."
  (unless (http-sse-client-p client)
    (%sse-protocol-error "Expected an HTTP-SSE-CLIENT value." client))
  (let ((was-active (http-sse-client-connected-p client)))
    (%sse-client-cancel-active-transport client (or reason :stopped))
    (setf (http-sse-client-stopped-p client) t
          (http-sse-client-connected-p client) nil
          (http-sse-client-parser client) nil
          (http-sse-client-next-retry-delay client) nil
          (http-sse-client-ready-state client) +http-sse-client-closed+)
    (when was-active
      (%sse-client-notify-close-reason client (or reason :stopped))))
  client)

(defun start-http-sse-client-response/k (client response on-success on-error)
  (%sse-call/k (lambda () (start-http-sse-client-response client response))
               on-success on-error))

(defun consume-http-sse-client-response/k
    (client response on-success on-error &optional reason)
  (%sse-call/k
   (lambda () (consume-http-sse-client-response client response reason))
   on-success on-error))

(defun feed-http-sse-client/k
    (client input on-success on-error &key (start 0) end)
  (%sse-call/k
   (lambda () (feed-http-sse-client client input :start start :end end))
   on-success on-error))

(defun finish-http-sse-client-response/k
    (client on-success on-error &optional reason)
  (%sse-call/k
   (lambda () (finish-http-sse-client-response client reason))
   on-success on-error))

(defun stop-http-sse-client/k
    (client on-success on-error &optional reason)
  (%sse-call/k
   (lambda () (stop-http-sse-client client reason))
   on-success on-error))

(defmacro with-http-sse-client ((var &rest options) &body body)
  `(let ((,var (make-http-sse-client ,@options)))
     (unwind-protect
          (progn ,@body)
       (unless (http-sse-client-stopped-p ,var)
         (stop-http-sse-client ,var :scope-exit)))))
