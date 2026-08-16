(in-package #:sse-kit)

(defconstant +http-sse-default-max-events+ 10000)

(defconstant +http-sse-default-max-line-bytes+ 65536)

(defconstant +http-sse-default-max-data-bytes+ (* 16 1024 1024))

(defconstant +http-sse-default-max-comments+ 10000)

(defconstant +http-sse-default-max-comment-bytes+ (* 1024 1024))

(defconstant +http-sse-default-max-input-bytes+ (* 64 1024 1024))

(defun %sse-octet-vector-p (value)
  (and (arrayp value)
       (= (array-rank value) 1)
       (not (stringp value))
       (loop for octet across value
             always (and (integerp octet) (<= 0 octet #xff)))))

(defun %sse-protocol-error (message &optional detail)
  (error 'sse-error
         :message message
         :operation :sse
         :detail detail))

(defun %sse-make-size-error (message limit observed kind)
  (make-condition 'sse-size-limit-exceeded
                  :message message
                  :operation :sse
                  :detail (list :kind kind :limit limit :observed observed)
                  :limit limit
                  :observed observed
                  :kind kind))

(defun %sse-size-error (message limit observed kind)
  (error (%sse-make-size-error message limit observed kind)))

(defun %sse-validate-limit (value name)
  (unless (or (null value)
              (and (integerp value) (>= value 0)))
    (%sse-protocol-error
     (format nil "~A must be NIL or a non-negative integer." name)
     value))
  value)

(defun %sse-no-line-breaks-p (value)
  (and (stringp value)
       (not (find-if (lambda (character)
                       (or (char= character #\Return)
                           (char= character #\Linefeed)))
                     value))))

(defun %sse-comment-sequence (comments)
  (cond ((null comments) nil)
        ((stringp comments)
         (unless (%sse-no-line-breaks-p comments)
           (%sse-protocol-error
            "SSE comments must be strings without line breaks."
            comments))
         (list comments))
        ((consp comments)
         (let ((slow comments)
               (fast comments))
           (loop
             (when (null fast)
               (return))
             (unless (consp fast)
               (%sse-protocol-error
                "SSE comments must be a proper finite list of strings."
                comments))
             (setf fast (cdr fast))
             (when (null fast)
               (return))
             (unless (consp fast)
               (%sse-protocol-error
                "SSE comments must be a proper finite list of strings."
                comments))
             (setf fast (cdr fast)
                   slow (cdr slow))
             (when (eq slow fast)
               (%sse-protocol-error
                "SSE comments must not be circular."
                comments))))
         (do ((tail comments (cdr tail)))
             ((null tail) comments)
           (unless (and (consp tail)
                        (stringp (car tail))
                        (%sse-no-line-breaks-p (car tail)))
             (%sse-protocol-error
              "SSE comments must be strings without line breaks."
              comments))))
        (t
         (%sse-protocol-error
          "SSE comments must be a string or a proper finite list of strings."
          comments))))

(defun %sse-normalize-comments (comments)
  (let ((sequence (%sse-comment-sequence comments)))
    (if (consp comments)
        (copy-list sequence)
        sequence)))

(defstruct (http-sse-event
             (:constructor %make-http-sse-event
                 (&key event data id last-event-id retry comments)))
  event
  data
  id
  last-event-id
  retry
  comments)

(defun make-http-sse-event
    (&key (event "message") (data "") id (last-event-id id) retry comments)
  "Construct a server-sent event value.

EVENT and DATA are strings.  ID and RETRY are optional; COMMENTS may be a
string or a list of strings and is emitted as SSE comment lines."
  (unless (and (stringp event) (%sse-no-line-breaks-p event))
    (%sse-protocol-error
     "An SSE event name must be a string without line breaks."
     event))
  (unless (stringp data)
    (%sse-protocol-error "An SSE event data value must be a string." data))
  (when (and id
             (or (not (stringp id))
                 (not (%sse-no-line-breaks-p id))
                 (find #\Null id :test #'char=)))
    (%sse-protocol-error
     "An SSE event ID must be a string without line breaks or NUL."
     id))
  (when (and last-event-id
             (or (not (stringp last-event-id))
                 (not (%sse-no-line-breaks-p last-event-id))
                 (find #\Null last-event-id :test #'char=)))
    (%sse-protocol-error
     "An SSE last-event-id value must be a string without line breaks or NUL."
     last-event-id))
  (when (and retry
             (or (not (integerp retry)) (minusp retry)))
    (%sse-protocol-error
     "An SSE retry value must be a non-negative integer or NIL."
     retry))
  (%make-http-sse-event
   :event event
   :data data
   :id id
   :last-event-id last-event-id
   :retry retry
   :comments (%sse-normalize-comments comments)))

(defun %sse-copy-http-sse-event (event)
  (unless (http-sse-event-p event)
    (%sse-protocol-error "Expected an HTTP-SSE-EVENT value." event))
  (let ((copy
          (make-http-sse-event
           :event (http-sse-event-event event)
           :data (http-sse-event-data event)
           :id (http-sse-event-id event)
           :last-event-id (http-sse-event-last-event-id event)
           :retry (http-sse-event-retry event)
           :comments (http-sse-event-comments event))))
    (%make-http-sse-event
     :event (copy-seq (http-sse-event-event copy))
     :data (copy-seq (http-sse-event-data copy))
     :id (and (http-sse-event-id copy)
              (copy-seq (http-sse-event-id copy)))
     :last-event-id (and (http-sse-event-last-event-id copy)
                         (copy-seq (http-sse-event-last-event-id copy)))
     :retry (http-sse-event-retry copy)
     :comments (mapcar #'copy-seq (http-sse-event-comments copy)))))

(defun %sse-utf8-string (octets)
  (unless (%sse-octet-vector-p octets)
    (%sse-protocol-error "SSE input lines must contain octets." octets))
  (cl-codec-kit:octets-to-string octets
                                 :encoding :utf-8
                                 :errorp nil))
