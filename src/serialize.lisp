(in-package #:sse-kit)

(defun %sse-append-octets (result octets)
  (loop for byte across octets
        do (vector-push-extend byte result))
  result)

(defun %sse-append-string (result string)
  (%sse-append-octets result (%sse-utf8-octets string)))

(defun %sse-emit-line (result prefix value)
  (%sse-append-string result prefix)
  (when value
    (%sse-append-string result value))
  (%sse-append-octets result #(13 10)))

(defun %sse-do-string-lines (string function)
  (let ((start 0)
        (length (length string))
        (index 0))
    (loop while (< index length)
          do (if (or (char= (char string index) #\Return)
                     (char= (char string index) #\Linefeed))
                 (progn
                   (funcall function string start index)
                   (when (and (char= (char string index) #\Return)
                              (< (1+ index) length)
                              (char= (char string (1+ index)) #\Linefeed))
                     (incf index))
                   (incf index)
                   (setf start index))
                 (incf index)))
    (funcall function string start length)))

(defun %sse-serialized-line-size (prefix value)
  (+ (%sse-utf8-octet-length prefix)
     (if value (%sse-utf8-octet-length value) 0)
     2))

(defun %sse-serialized-line-range-size (prefix string start end)
  (+ (%sse-utf8-octet-length prefix)
     (%sse-utf8-octet-length string :start start :end end)
     2))

(defun %sse-serialized-event-size (event-name data id retry comments)
  (let ((size 0))
    (dolist (comment comments)
      (incf size (%sse-serialized-line-size ":" comment)))
    (when (and (plusp (length event-name))
               (not (string= event-name "message")))
      (incf size (%sse-serialized-line-size "event:" event-name)))
    (when id
      (incf size (%sse-serialized-line-size "id:" id)))
    (when (not (null retry))
      (incf size
            (%sse-serialized-line-size "retry:" (princ-to-string retry))))
    (%sse-do-string-lines
     data
     (lambda (string start end)
       (incf size (%sse-serialized-line-range-size
                   "data:" string start end))))
    (+ size 2)))

(defun %sse-serialization-fields (event)
  (unless (http-sse-event-p event)
    (%sse-protocol-error "Expected an HTTP-SSE-EVENT value." event))
  (let ((event-name (http-sse-event-event event))
        (data (http-sse-event-data event))
        (id (http-sse-event-id event))
        (retry (http-sse-event-retry event))
        (comments (%sse-comment-sequence
                   (http-sse-event-comments event))))
    (unless (and (stringp event-name) (%sse-no-line-breaks-p event-name))
      (%sse-protocol-error
       "An SSE event name must be a string without line breaks."
       event-name))
    (unless (stringp data)
      (%sse-protocol-error "An SSE event data value must be a string." data))
    (when (and id
               (or (not (stringp id))
                   (not (%sse-no-line-breaks-p id))
                   (find #\Null id :test #'char=)))
      (%sse-protocol-error
       "An SSE event ID must be a string without line breaks or NUL."
       id))
    (when (and retry
               (or (not (integerp retry)) (minusp retry)))
      (%sse-protocol-error
       "An SSE retry value must be a non-negative integer or NIL."
       retry))
    (values event-name data id retry comments)))

(defun %sse-emit-line-range (result prefix string start end)
  (%sse-append-string result prefix)
  (%sse-append-octets
   result
   (%sse-utf8-octets string :start start :end end))
  (%sse-append-octets result #(13 10)))

(defun %sse-serialize-event-fields (result event-name data id retry comments)
  (dolist (comment comments)
    (%sse-emit-line result ":" comment))
  (when (and (plusp (length event-name))
             (not (string= event-name "message")))
    (%sse-emit-line result "event:" event-name))
  (when id
    (%sse-emit-line result "id:" id))
  (when (not (null retry))
    (%sse-emit-line result "retry:" (princ-to-string retry)))
  (%sse-do-string-lines
   data
   (lambda (string start end)
     (%sse-emit-line-range result "data:" string start end)))
  (%sse-emit-line result "" nil)
  result)

(defun serialize-http-sse-event (event &key max-bytes)
  "Serialize one HTTP-SSE-EVENT as UTF-8 octets ending in a blank line.

When MAX-BYTES is non-NIL, signal SSE-SIZE-LIMIT-EXCEEDED before returning a
  larger output vector."
  (%sse-validate-limit max-bytes "MAX-BYTES")
  (multiple-value-bind (event-name data id retry comments)
      (%sse-serialization-fields event)
    (let ((size (%sse-serialized-event-size
                 event-name data id retry comments)))
      (when (and max-bytes (> size max-bytes))
        (%sse-size-error
         "Serialized SSE event exceeds MAX-BYTES."
         max-bytes
         size
         :output-bytes))
      (let ((result (make-array size
                                :element-type '(unsigned-byte 8)
                                :adjustable t
                                :fill-pointer 0)))
        (%sse-serialize-event-fields result event-name data id retry comments)
        (let ((copy (make-array (length result)
                                :element-type '(unsigned-byte 8))))
          (replace copy result)
          copy)))))

(defun write-http-sse-event (event destination &key max-bytes)
  "Write EVENT's serialized octets to DESTINATION and return DESTINATION.

DESTINATION may be a binary stream or a function accepting one octet vector.
The function form lets transport adapters preserve backpressure and partial
write policy without making the core depend on a socket implementation."
  (let ((octets (serialize-http-sse-event event :max-bytes max-bytes)))
    (cond ((functionp destination)
          (funcall destination octets))
          ((streamp destination)
           (unless (%sse-binary-stream-p destination)
             (%sse-protocol-error
              "An SSE destination stream must accept octets."
              (stream-element-type destination)))
           (write-sequence octets destination))
          (t
           (%sse-protocol-error
            "An SSE destination must be a stream or function."
            destination)))
    destination))
