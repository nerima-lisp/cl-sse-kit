(in-package #:sse-kit)

(defun %sse-binary-stream-p (stream)
  (and (streamp stream)
       (multiple-value-bind (subtype-p known-p)
           (subtypep '(unsigned-byte 8)
                     (stream-element-type stream))
         (and known-p subtype-p))))

(defun %sse-character-stream-p (stream)
  (and (streamp stream)
       (handler-case
           (nth-value 0 (subtypep (stream-element-type stream) 'character))
         (error () nil))))

(defun %sse-string-range (string start end)
  (unless (and (integerp start)
               (<= 0 start)
               (integerp end)
               (<= start end (length string)))
    (%sse-protocol-error
     "UTF-8 encoding START and END must describe a valid string range."
     (list :start start :end end :length (length string))))
  end)

(defun %sse-utf8-octet-length (string &key (start 0) end)
  (unless (stringp string)
    (%sse-protocol-error "UTF-8 encoding requires a string." string))
  (let ((end (or end (length string))))
    (%sse-string-range string start end)
    (handler-case
        (cl-codec-kit:string-size-in-octets string
                                            :start start
                                            :end end
                                            :encoding :utf-8)
      (cl-codec-kit:unencodable-character (condition)
        (%sse-protocol-error
         "A string contains a character that cannot be encoded as UTF-8."
         condition)))))

(defun %sse-utf8-octets (string &key (start 0) end)
  "Encode STRING as UTF-8 octets using the project codec."
  (unless (stringp string)
    (%sse-protocol-error "UTF-8 encoding requires a string." string))
  (let ((end (or end (length string))))
    (%sse-string-range string start end)
    (handler-case
        (cl-codec-kit:string-to-octets string
                                       :start start
                                       :end end
                                       :encoding :utf-8
                                       :errorp t)
      (cl-codec-kit:unencodable-character (condition)
        (%sse-protocol-error
         "A string contains a character that cannot be encoded as UTF-8."
         condition)))))
