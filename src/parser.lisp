(in-package #:sse-kit)

(defstruct (%sse-state
             (:constructor %make-sse-state
                 (&key max-events max-line-bytes max-data-bytes max-comments
                       max-comment-bytes max-input-bytes on-event
                       collect-events-p last-event-id)))
  max-events
  max-line-bytes
  max-data-bytes
  max-comments
  max-comment-bytes
  max-input-bytes
  on-event
  collect-events-p
  (event-field nil)
  (data-lines nil)
  (data-bytes 0)
  (id-field nil)
  (retry-field nil)
  (comments nil)
  (comment-bytes 0)
  (last-event-id "")
  (retry-time nil)
  (line (make-array 0
                    :element-type '(unsigned-byte 8)
                    :adjustable t
                    :fill-pointer 0))
  (first-line-p t)
  (pending-cr-p nil)
  (event-count 0)
  (input-bytes 0)
  (events nil))

(defun %sse-make-state
    (&key max-events max-line-bytes max-data-bytes max-comments
          max-comment-bytes max-input-bytes on-event collect-events-p
          initial-last-event-id)
  (%sse-validate-limit max-events "MAX-EVENTS")
  (%sse-validate-limit max-line-bytes "MAX-LINE-BYTES")
  (%sse-validate-limit max-data-bytes "MAX-DATA-BYTES")
  (%sse-validate-limit max-comments "MAX-COMMENTS")
  (%sse-validate-limit max-comment-bytes "MAX-COMMENT-BYTES")
  (%sse-validate-limit max-input-bytes "MAX-INPUT-BYTES")
  (unless (and (stringp initial-last-event-id)
               (%sse-no-line-breaks-p initial-last-event-id)
               (not (find #\Null initial-last-event-id :test #'char=)))
    (%sse-protocol-error
     "INITIAL-LAST-EVENT-ID must be a string without line breaks or NUL."
     initial-last-event-id))
  (when (and on-event (not (functionp on-event)))
    (%sse-protocol-error "ON-EVENT must be a function or NIL." on-event))
  (%make-sse-state
   :max-events max-events
   :max-line-bytes max-line-bytes
   :max-data-bytes max-data-bytes
   :max-comments max-comments
   :max-comment-bytes max-comment-bytes
   :max-input-bytes max-input-bytes
   :on-event on-event
   :collect-events-p collect-events-p
   :last-event-id initial-last-event-id))

(defun %sse-state-reset-event (state)
  (setf (%sse-state-event-field state) nil
        (%sse-state-data-lines state) nil
        (%sse-state-data-bytes state) 0
        (%sse-state-id-field state) nil
        (%sse-state-retry-field state) nil
        (%sse-state-comments state) nil
        (%sse-state-comment-bytes state) 0)
  state)

(defun %sse-state-event-data (state)
  (with-output-to-string (result)
    (loop for data-line in (reverse (%sse-state-data-lines state))
          for firstp = t then nil
          do (unless firstp
               (write-char #\Linefeed result))
             (write-string data-line result))))

(defun %sse-state-build-event (state)
  (%make-http-sse-event
   :event (if (and (%sse-state-event-field state)
                   (plusp (length (%sse-state-event-field state))))
              (%sse-state-event-field state)
              "message")
   :data (%sse-state-event-data state)
   :id (%sse-state-id-field state)
   :last-event-id (%sse-state-last-event-id state)
   :retry (%sse-state-retry-field state)
   :comments (reverse (%sse-state-comments state))))

(defun %sse-state-record-event (state event)
  (incf (%sse-state-event-count state))
  (when (%sse-state-collect-events-p state)
    (push event (%sse-state-events state)))
  (%sse-state-reset-event state)
  (when (%sse-state-on-event state)
    (funcall (%sse-state-on-event state) event))
  state)

(defun %sse-state-dispatch (state)
  (if (consp (%sse-state-data-lines state))
      (progn
        (when (and (%sse-state-max-events state)
                   (>= (%sse-state-event-count state)
                       (%sse-state-max-events state)))
          (%sse-size-error
           "An SSE input exceeded its event-count limit."
           (%sse-state-max-events state)
           (1+ (%sse-state-event-count state))
           :events))
        (%sse-state-record-event state (%sse-state-build-event state)))
      (%sse-state-reset-event state))
  state)

(defun %sse-parse-retry (octets)
  (when (and (plusp (length octets))
             (loop for byte across octets
                   always (<= #x30 byte #x39)))
    (let ((value 0))
      (loop for byte across octets
            for digit = (- byte #x30)
            do (setf value (+ (* value 10) digit)))
      value)))

(defun %sse-state-check-comment (state octet-count)
  (when (and (%sse-state-max-comments state)
             (>= (length (%sse-state-comments state))
                 (%sse-state-max-comments state)))
    (%sse-size-error
     "An SSE event exceeded its comment-count limit."
     (%sse-state-max-comments state)
     (1+ (length (%sse-state-comments state)))
     :comments))
  (when (and (%sse-state-max-comment-bytes state)
             (> (+ (%sse-state-comment-bytes state) octet-count)
                (%sse-state-max-comment-bytes state)))
    (%sse-size-error
     "An SSE event exceeded its comment-size limit."
     (%sse-state-max-comment-bytes state)
     (+ (%sse-state-comment-bytes state) octet-count)
     :comment-bytes)))

(defun %sse-state-line-start (state line)
  (let ((start 0)
        (end (length line)))
    (when (%sse-state-first-line-p state)
      (setf (%sse-state-first-line-p state) nil)
      (when (and (>= end 3)
                 (= (aref line 0) #xef)
                 (= (aref line 1) #xbb)
                 (= (aref line 2) #xbf))
        (setf start 3)))
    start))

(defun %sse-state-process-comment (state line start end)
  (let ((comment-octets (subseq line (1+ start) end)))
    (%sse-state-check-comment state (length comment-octets))
    (incf (%sse-state-comment-bytes state) (length comment-octets))
    (push (%sse-utf8-string comment-octets)
          (%sse-state-comments state))))

(defun %sse-line-field-parts (line start end)
  (let* ((colon (loop for index from start below end
                      when (= (aref line index) #x3a)
                        return index))
         (field-end (or colon end))
         (value-start (if colon (1+ colon) end)))
    (when (and (< value-start end)
               (= (aref line value-start) #x20))
      (incf value-start))
    (values (subseq line start field-end)
            (subseq line value-start end))))

(defun %sse-state-process-data (state value-octets)
  (let ((addition
          (+ (length value-octets)
             (if (consp (%sse-state-data-lines state)) 1 0))))
    (when (and (%sse-state-max-data-bytes state)
               (> (+ (%sse-state-data-bytes state) addition)
                  (%sse-state-max-data-bytes state)))
      (%sse-size-error
       "An SSE event exceeded its data-size limit."
       (%sse-state-max-data-bytes state)
       (+ (%sse-state-data-bytes state) addition)
       :data))
    (incf (%sse-state-data-bytes state) addition)
    (push (%sse-utf8-string value-octets)
          (%sse-state-data-lines state))))

(defun %sse-state-process-field (state field value-octets)
  (cond ((string= field "data")
         (%sse-state-process-data state value-octets))
        ((string= field "event")
         (setf (%sse-state-event-field state)
               (%sse-utf8-string value-octets)))
        ((string= field "id")
         (unless (find #x00 value-octets)
           (let ((id (%sse-utf8-string value-octets)))
             (setf (%sse-state-id-field state) id
                   (%sse-state-last-event-id state) id))))
        ((string= field "retry")
         (let ((retry (%sse-parse-retry value-octets)))
           (when retry
             (setf (%sse-state-retry-field state) retry
                   (%sse-state-retry-time state) retry))))))

(defun %sse-state-process-line (state line)
  (let* ((start (%sse-state-line-start state line))
         (end (length line)))
    (cond ((= start end)
           (%sse-state-dispatch state))
          ((= (aref line start) #x3a)
           (%sse-state-process-comment state line start end))
          (t
           (multiple-value-bind (field-octets value-octets)
               (%sse-line-field-parts line start end)
             (%sse-state-process-field
              state (%sse-utf8-string field-octets) value-octets)))))
  state)

(defun %sse-state-finish-line (state)
  (let ((line (%sse-state-line state)))
    (%sse-state-process-line state line)
    (setf (fill-pointer line) 0))
  state)

(defun %sse-state-append-byte (state byte)
  (unless (and (integerp byte) (<= 0 byte #xff))
    (%sse-protocol-error "An SSE input byte is outside the octet range." byte))
  (when (and (%sse-state-max-line-bytes state)
             (>= (length (%sse-state-line state))
                 (%sse-state-max-line-bytes state)))
    (%sse-size-error
     "An SSE input line exceeded its size limit."
     (%sse-state-max-line-bytes state)
     (1+ (length (%sse-state-line state)))
     :line))
  (vector-push-extend byte (%sse-state-line state))
  state)

(defun %sse-check-input-limit (state octet-count)
  (when (and (%sse-state-max-input-bytes state)
             (> (+ (%sse-state-input-bytes state) octet-count)
                (%sse-state-max-input-bytes state)))
    (%sse-size-error
     "An SSE input exceeded its total-size limit."
     (%sse-state-max-input-bytes state)
     (+ (%sse-state-input-bytes state) octet-count)
     :input-bytes))
  state)

(defun %sse-state-feed-byte (state byte)
  (unless (and (integerp byte) (<= 0 byte #xff))
    (%sse-protocol-error "An SSE input byte is outside the octet range." byte))
  (%sse-check-input-limit state 1)
  (incf (%sse-state-input-bytes state))
  (loop
    (when (%sse-state-pending-cr-p state)
      (setf (%sse-state-pending-cr-p state) nil)
      (%sse-state-finish-line state)
      (when (= byte #x0a)
        (return state)))
    (cond ((= byte #x0d)
           (setf (%sse-state-pending-cr-p state) t)
           (return state))
          ((= byte #x0a)
           (%sse-state-finish-line state)
           (return state))
          (t
           (%sse-state-append-byte state byte)
           (return state)))))

(defun %sse-state-finish (state)
  (when (%sse-state-pending-cr-p state)
    (setf (%sse-state-pending-cr-p state) nil)
    (%sse-state-finish-line state))
  (setf (fill-pointer (%sse-state-line state)) 0)
  (%sse-state-reset-event state)
  state)

(defun %sse-state-result (state)
  (when (%sse-state-collect-events-p state)
    (nreverse (copy-list (%sse-state-events state)))))

(defstruct (http-sse-parser
             (:constructor %make-http-sse-parser (state)))
  state
  (finished-p nil))

(defun make-http-sse-parser
    (&key (max-events +http-sse-default-max-events+)
          (max-line-bytes +http-sse-default-max-line-bytes+)
          (max-data-bytes +http-sse-default-max-data-bytes+)
          (max-comments +http-sse-default-max-comments+)
          (max-comment-bytes +http-sse-default-max-comment-bytes+)
          (max-input-bytes +http-sse-default-max-input-bytes+)
          on-event (collect-events-p t) (initial-last-event-id ""))
  "Create an incremental SSE parser.

FEED-HTTP-SSE-PARSER accepts UTF-8 strings or octet vectors.  The parser
follows the WHATWG EOF rule: an event is dispatched only after a blank line."
  (%make-http-sse-parser
   (%sse-make-state
    :max-events max-events
    :max-line-bytes max-line-bytes
    :max-data-bytes max-data-bytes
    :max-comments max-comments
    :max-comment-bytes max-comment-bytes
    :max-input-bytes max-input-bytes
    :on-event on-event
    :collect-events-p collect-events-p
    :initial-last-event-id initial-last-event-id)))

(defun %sse-check-range (input start end)
  (let ((length (length input)))
    (unless (and (integerp start) (<= 0 start length)
                 (integerp end) (<= start end length))
      (%sse-protocol-error
       "SSE chunk START and END must describe a valid input range."
       (list :start start :end end :length length)))))

(defun %sse-feed-octets (parser octets)
  (let ((state (http-sse-parser-state parser)))
    (%sse-check-input-limit state (length octets))
    (loop for byte across octets
          do (%sse-state-feed-byte state byte)))
  parser)

(defun %sse-feed-string-chunk (parser input start end)
  (%sse-check-range input start end)
  (let ((state (http-sse-parser-state parser)))
    (%sse-check-input-limit
     state
     (%sse-utf8-octet-length input :start start :end end))
    (%sse-feed-octets
     parser
     (%sse-utf8-octets input :start start :end end))))

(defun %sse-feed-octet-chunk (parser input start end)
  (%sse-check-range input start end)
  (let ((state (http-sse-parser-state parser)))
    (%sse-check-input-limit state (- end start))
    (loop for index from start below end
          do (%sse-state-feed-byte state (aref input index))))
  parser)

(defun feed-http-sse-parser (parser input &key (start 0) end)
  "Feed one chunk into PARSER and return PARSER.

START and END are character bounds for strings and octet bounds for vectors.
An octet vector may split a UTF-8 sequence or CRLF pair across calls."
  (unless (http-sse-parser-p parser)
    (%sse-protocol-error "Expected an HTTP-SSE-PARSER value." parser))
  (when (http-sse-parser-finished-p parser)
    (%sse-protocol-error "Cannot feed an SSE parser after it is finished."
                         parser))
  (cond ((stringp input)
         (let ((chunk-end (or end (length input))))
           (%sse-feed-string-chunk parser input start chunk-end)))
        ((%sse-octet-vector-p input)
         (let ((chunk-end (or end (length input))))
           (%sse-feed-octet-chunk parser input start chunk-end)))
        (t
         (%sse-protocol-error
          "SSE input chunks must be strings or vectors of octets."
          input))))

(defun finish-http-sse-parser (parser)
  "Finish PARSER and discard any event not terminated by a blank line."
  (unless (http-sse-parser-p parser)
    (%sse-protocol-error "Expected an HTTP-SSE-PARSER value." parser))
  (unless (http-sse-parser-finished-p parser)
    (%sse-state-finish (http-sse-parser-state parser))
    (setf (http-sse-parser-finished-p parser) t))
  parser)

(defun http-sse-parser-last-event-id (parser)
  (unless (http-sse-parser-p parser)
    (%sse-protocol-error "Expected an HTTP-SSE-PARSER value." parser))
  (%sse-state-last-event-id (http-sse-parser-state parser)))

(defun http-sse-parser-retry (parser)
  (unless (http-sse-parser-p parser)
    (%sse-protocol-error "Expected an HTTP-SSE-PARSER value." parser))
  (%sse-state-retry-time (http-sse-parser-state parser)))

(defun http-sse-parser-event-count (parser)
  (unless (http-sse-parser-p parser)
    (%sse-protocol-error "Expected an HTTP-SSE-PARSER value." parser))
  (%sse-state-event-count (http-sse-parser-state parser)))

(defun http-sse-parser-events (parser)
  (unless (http-sse-parser-p parser)
    (%sse-protocol-error "Expected an HTTP-SSE-PARSER value." parser))
  (%sse-state-result (http-sse-parser-state parser)))

(defun parse-http-sse-events
    (input &key (max-events +http-sse-default-max-events+)
                 (max-line-bytes +http-sse-default-max-line-bytes+)
                 (max-data-bytes +http-sse-default-max-data-bytes+)
                 (max-comments +http-sse-default-max-comments+)
                 (max-comment-bytes +http-sse-default-max-comment-bytes+)
                 (max-input-bytes +http-sse-default-max-input-bytes+)
                 (initial-last-event-id ""))
  "Parse a string or octet vector into HTTP-SSE-EVENT values.

An event is returned only when the input contains the blank line required by
the SSE processing model."
  (let ((parser
          (make-http-sse-parser
           :max-events max-events
           :max-line-bytes max-line-bytes
           :max-data-bytes max-data-bytes
           :max-comments max-comments
           :max-comment-bytes max-comment-bytes
           :max-input-bytes max-input-bytes
           :initial-last-event-id initial-last-event-id
           :collect-events-p t)))
    (feed-http-sse-parser parser input)
    (finish-http-sse-parser parser)
    (http-sse-parser-events parser)))

(defun feed-http-sse-parser/k (parser input on-success on-error &key (start 0) end)
  (%sse-call/k
   (lambda ()
     (feed-http-sse-parser parser input :start start :end end))
   on-success
   on-error))

(defun finish-http-sse-parser/k (parser on-success on-error)
  (%sse-call/k
   (lambda () (finish-http-sse-parser parser))
   on-success
   on-error))

(defmacro with-http-sse-parser ((var &rest options) &body body)
  `(let ((,var (make-http-sse-parser ,@options)))
     ,@body))
