(in-package #:sse-kit/test)

(describe "parse-http-sse-events"
  (it "reads every field of a single event"
    (let ((event (first (parse-http-sse-events
                         (lf "event: greet" "data: hello" "data: world"
                             "id: 7" "retry: 250" "")))))
      (expect (http-sse-event-event event) :to-equalp "greet")
      (expect (http-sse-event-id event) :to-equalp "7")
      (expect (http-sse-event-retry event) :to-equalp 250)))

  ;; Successive data lines belong to one event and are joined with a line
  ;; feed, never with the line ending that separated them on the wire.
  (it "joins successive data lines with a line feed"
    (let ((event (first (parse-http-sse-events
                         (lf "data: hello" "data: world" "")))))
      (expect (http-sse-event-data event) :to-equalp (format nil "hello~%world"))))

  (it "splits events on a blank line"
    (let ((events (parse-http-sse-events (lf "data: one" "" "data: two" ""))))
      (expect (length events) :to-equalp 2)
      (expect (http-sse-event-data (first events)) :to-equalp "one")
      (expect (http-sse-event-data (second events)) :to-equalp "two")))

  ;; A leading colon marks a comment. It is retained for callers that use
  ;; comments as keep-alives, but must never leak into the data.
  (it "keeps a comment out of the data and records it separately"
    (let ((event (first (parse-http-sse-events
                         (format nil ": keep-alive~%data: x~%~%")))))
      (expect (http-sse-event-data event) :to-equalp "x")
      (expect (http-sse-event-comments event) :to-equalp (list " keep-alive"))))

  (it "accepts CRLF line endings"
    (expect (http-sse-event-data
             (first (parse-http-sse-events (crlf "data: crlf" ""))))
            :to-equalp "crlf"))

  (it "accepts a bare CR line ending"
    (expect (http-sse-event-data
             (first (parse-http-sse-events
                     (format nil "data: cr~C~C" #\Return #\Return))))
            :to-equalp "cr"))

  ;; Exactly one space after the colon is optional padding; a second space is
  ;; part of the value.
  (it "strips at most one space after the colon"
    (expect (http-sse-event-data
             (first (parse-http-sse-events (lf "data:  two-spaces" ""))))
            :to-equalp " two-spaces"))

  (it "accepts an octet vector as well as a string"
    (expect (http-sse-event-data
             (first (parse-http-sse-events
                     (map '(vector (unsigned-byte 8)) #'char-code
                          (lf "data: bytes" "")))))
            :to-equalp "bytes"))

  ;; A retry value that is not a base-ten integer is ignored rather than
  ;; fatal, so one malformed field cannot end the stream.
  (it "ignores a non-numeric retry"
    (expect (http-sse-event-retry
             (first (parse-http-sse-events (lf "retry: soon" "data: x" ""))))
            :to-equalp nil))

  ;; A block that never dispatches (e.g. a standalone keep-alive comment)
  ;; must not leak its comments, event name, id, or retry into whichever
  ;; event is dispatched next.
  (it "does not leak a comment-only block's fields into the next event"
    (let ((events (parse-http-sse-events
                   (lf ": keep-alive" "" "data: two" ""))))
      (expect (length events) :to-equalp 1)
      (expect (http-sse-event-comments (first events)) :to-equalp nil)))

  (it "accepts a retry value beyond the fixnum range"
    (let ((digits (make-string 128 :initial-element #\9)))
      (let ((retry
              (http-sse-event-retry
               (first (parse-http-sse-events
                       (lf (concatenate 'string "retry: " digits)
                           "data: x"
                           ""))))))
        (expect (integerp retry) :to-equalp t)
        (expect retry :to-equalp (parse-integer digits))))))

(describe "parser limits"
  (it "rejects more events than max-events allows"
    (signals sse-size-limit-exceeded
      (parse-http-sse-events (lf "data: a" "" "data: b" "") :max-events 1)))

  (it "rejects a line longer than max-line-bytes"
    (signals sse-size-limit-exceeded
      (parse-http-sse-events (lf "data: aaaaaaaaaa" "") :max-line-bytes 4)))

  (it "rejects accumulated data larger than max-data-bytes"
    (signals sse-size-limit-exceeded
      (parse-http-sse-events (lf "data: aaaaaaaaaa" "") :max-data-bytes 4)))

  (it "counts UTF-8 octets before accepting string chunks"
    (signals sse-size-limit-exceeded
      (feed-http-sse-parser
       (make-http-sse-parser :max-input-bytes 3)
       "éé"))))
