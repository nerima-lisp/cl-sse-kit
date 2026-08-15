(defpackage #:sse-kit
  (:use #:cl)
  (:export
   ;; Conditions
   #:sse-error
   #:sse-error-message
   #:sse-error-operation
   #:sse-error-detail
   #:sse-size-limit-exceeded
   #:sse-size-limit-exceeded-limit
   #:sse-size-limit-exceeded-observed
   #:sse-size-limit-exceeded-kind
   ;; Event values
   #:http-sse-event
   #:http-sse-event-p
   #:make-http-sse-event
   #:http-sse-event-event
   #:http-sse-event-data
   #:http-sse-event-id
   #:http-sse-event-retry
   #:http-sse-event-comments
   ;; Parsing and serialization
   #:parse-http-sse-events
   #:read-http-sse-events
   #:serialize-http-sse-event))
