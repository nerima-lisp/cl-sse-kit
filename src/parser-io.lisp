(in-package #:sse-kit)

(defun read-http-sse-events
    (stream &key (max-events +http-sse-default-max-events+)
                  (max-line-bytes +http-sse-default-max-line-bytes+)
                  (max-data-bytes +http-sse-default-max-data-bytes+)
                  (max-comments +http-sse-default-max-comments+)
                  (max-comment-bytes +http-sse-default-max-comment-bytes+)
                  (max-input-bytes +http-sse-default-max-input-bytes+)
                  on-event (collect-events-p t) (initial-last-event-id ""))
  "Read strict SSE events from a binary or character STREAM.

ON-EVENT is called for each event terminated by a blank line."
  (unless (streamp stream)
    (%sse-protocol-error "SSE input must be a stream." stream))
  (let ((parser
          (make-http-sse-parser
           :max-events max-events
           :max-line-bytes max-line-bytes
           :max-data-bytes max-data-bytes
           :max-comments max-comments
           :max-comment-bytes max-comment-bytes
           :max-input-bytes max-input-bytes
           :on-event on-event
           :collect-events-p collect-events-p
           :initial-last-event-id initial-last-event-id)))
    (if (%sse-character-stream-p stream)
        (let ((buffer (make-string 8192)))
          (loop for count = (read-sequence buffer stream)
                while (plusp count)
                do (feed-http-sse-parser parser buffer :end count)))
        (progn
          (unless (%sse-binary-stream-p stream)
            (%sse-protocol-error
             "An SSE input stream must produce characters or octets."
             (stream-element-type stream)))
          (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
            (loop for count = (read-sequence buffer stream)
                  while (plusp count)
                  do (feed-http-sse-parser parser buffer :end count)))))
    (finish-http-sse-parser parser)
    (http-sse-parser-events parser)))
