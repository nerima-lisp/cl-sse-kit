(in-package #:sse-kit/test)

(describe "serialize-http-sse-event"
  ;; Serialization produces UTF-8 octets rather than a string, because the
  ;; caller writes them to a binary body; and it uses CRLF, the line ending
  ;; the wire format is specified in.
  (it "emits every populated field as CRLF-terminated octets"
    (expect (octets-string
             (serialize-http-sse-event
              (make-http-sse-event :event "greet" :data "hello" :id "7"
                                   :retry 250)))
            :to-equalp (crlf "event:greet" "id:7" "retry:250" "data:hello" "")))

  (it "splits multi-line data across one data field per line"
    (expect (octets-string
             (serialize-http-sse-event
              (make-http-sse-event :data (format nil "a~%b"))))
            :to-equalp (crlf "data:a" "data:b" "")))

  (it "produces output the parser reads back unchanged"
    (expect (http-sse-event-data
             (first (parse-http-sse-events
                     (serialize-http-sse-event
                      (make-http-sse-event :data (format nil "a~%b"))))))
            :to-equalp (format nil "a~%b"))))

(describe "make-http-sse-event"
  ;; A field value carrying a line break would terminate the field early on
  ;; the wire and let a caller inject arbitrary fields, so it is refused at
  ;; construction rather than escaped at serialization.
  (it "rejects an event name containing a line break"
    (signals sse-error
      (make-http-sse-event :event (format nil "a~%b") :data "x"))))
