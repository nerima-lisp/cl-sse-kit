(in-package #:sse-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(make-http-sse-response make-http-sse-response-stream
           http-sse-request-valid-p validate-http-sse-request
           http-sse-request-last-event-id)
          "SSE-KIT"))

(defun %sse-http-token-character-p (character)
  (let ((code (char-code character)))
    (or (<= (char-code #\0) code (char-code #\9))
        (<= (char-code #\A) code (char-code #\Z))
        (<= (char-code #\a) code (char-code #\z))
        (find character "!#$%&'*+-.^_|~" :test #'char=)
        (= code #x60))))

(defun %sse-http-token-p (value)
  (and (stringp value)
       (plusp (length value))
       (every #'%sse-http-token-character-p value)))

(defun %sse-http-header-value-p (value)
  (and (stringp value)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (or (= code #x09)
                      (<= #x20 code #x7e)
                      (<= #x80 code #xff))))
              value)))

(defun %sse-http-quoted-text-character-p (character)
  (let ((code (char-code character)))
    (or (= code #x09)
        (= code #x20)
        (and (<= #x21 code #x7e)
             (/= code #x22)
             (/= code #x5c))
        (<= #x80 code #xff))))

(defun %sse-http-quoted-pair-character-p (character)
  (let ((code (char-code character)))
    (or (= code #x09)
        (<= #x20 code #x7e)
        (<= #x80 code #xff))))

(defun %sse-http-parameter-value (value)
  (cond
    ((%sse-http-token-p value)
     (values value t))
    ((and (stringp value)
          (>= (length value) 2)
          (char= (char value 0) #\")
          (char= (char value (1- (length value))) #\"))
     (let ((stream (make-string-output-stream))
           (index 1)
           (end (1- (length value))))
       (loop while (< index end)
             do (let ((character (char value index)))
                  (if (char= character #\\)
                      (progn
                        (incf index)
                        (when (or (>= index end)
                                  (not (%sse-http-quoted-pair-character-p
                                        (char value index))))
                          (return-from %sse-http-parameter-value
                            (values nil nil)))
                        (write-char (char value index) stream))
                      (progn
                        (unless (%sse-http-quoted-text-character-p character)
                          (return-from %sse-http-parameter-value
                            (values nil nil)))
                        (write-char character stream)))
                  (incf index)))
       (values (get-output-stream-string stream) t)))
    (t
     (values nil nil))))

(defun %sse-http-value-separator (value start end separator-character)
  (loop with quoted-p = nil
        with escaped-p = nil
        for index from start below end
        for character = (char value index)
        do (cond
             (escaped-p
              (setf escaped-p nil))
             ((and quoted-p (char= character #\\))
              (setf escaped-p t))
             ((char= character #\")
              (setf quoted-p (not quoted-p)))
             ((and (not quoted-p)
                   (char= character separator-character))
              (return-from %sse-http-value-separator
                (values index t))))
        finally
        (return (values end (not (or quoted-p escaped-p))))))

(defun %sse-content-type-parameter-valid-p (parameter charset-seen-p)
  (let* ((equals (position #\= parameter))
         (name (and equals
                    (subseq parameter 0 equals)))
         (value (and equals
                     (subseq parameter (1+ equals))))
         (charset-p (and name (string-equal name "charset"))))
    (multiple-value-bind (decoded-value value-valid-p)
        (%sse-http-parameter-value value)
      (declare (ignore decoded-value))
      (values
       (and (plusp (length parameter))
            equals
            (%sse-http-token-p name)
            value-valid-p
            (or (not charset-p)
                (not charset-seen-p)))
       (or charset-seen-p charset-p)))))

(defun %sse-content-type-parameters-valid-p (value start end)
  (and (< start end)
       (loop with charset-seen-p = nil
             with current = start
             do (when (= current end)
                  (return nil))
                (multiple-value-bind (separator syntax-valid-p)
                    (%sse-http-value-separator
                     value current end #\;)
                  (unless syntax-valid-p
                    (return nil))
                  (let ((parameter
                          (string-trim
                           '(#\Space #\Tab)
                           (subseq value current separator))))
                    (multiple-value-bind (valid-p next-charset-seen-p)
                        (%sse-content-type-parameter-valid-p
                         parameter charset-seen-p)
                      (unless valid-p
                        (return nil))
                      (if (= separator end)
                          (return t)
                          (setf charset-seen-p next-charset-seen-p
                                current (1+ separator)))))))))

(defun http-sse-content-type-valid-p (value)
  "Return true when VALUE has the text/event-stream media type essence."
  (when (%sse-http-header-value-p value)
    (let* ((length (length value))
           (separator (position #\; value))
           (media-type (string-trim '(#\Space #\Tab)
                                    (subseq value 0 (or separator length)))))
      (and (string-equal media-type "text/event-stream")
           (or (null separator)
               (%sse-content-type-parameters-valid-p
               value (1+ separator) length))))))

(defun %sse-http-single-header-value (headers name)
  (let ((values (http-message-kit:http-header-values headers name)))
    (when (= (length values) 1)
      (first values))))

(defun %sse-http-header-value-from-utf8 (value)
  (map 'string #'code-char (%sse-utf8-octets value)))

(defun %sse-http-header-value-to-utf8 (value)
  (%sse-utf8-string (map 'vector #'char-code value)))

(defun %sse-http-managed-header-p (name)
  (member (string-downcase name)
          '("content-type"
            "cache-control"
            "access-control-allow-origin"
            "access-control-allow-credentials"
            "x-accel-buffering"
            "vary")
          :test #'string=))

(defun %sse-http-user-headers (headers)
  (let* ((probe (http-message-kit:make-http-response
                  :status 200
                  :headers headers))
         (normalized (http-message-kit:http-response-headers probe)))
    (when (some (lambda (header)
                  (%sse-http-managed-header-p
                   (http-message-kit:http-header-name header)))
                normalized)
      (%sse-protocol-error
       "SSE response headers cannot replace managed headers."
       normalized))
    normalized))

(defun %sse-http-cors-origin (value)
  (when value
    (unless (and (stringp value)
                 (%sse-http-header-value-p value))
      (%sse-protocol-error
       "CORS origin must be a valid HTTP header value."
       value))
    (let ((origin (string-trim '(#\Space #\Tab) value)))
      (unless (and (plusp (length origin))
                   (not (find #\, origin)))
        (%sse-protocol-error
         "CORS origin must contain one non-empty origin."
         value))
      origin)))

(defun %sse-http-cors-credentials-p (value origin)
  (unless (or (null value) (eq value t))
    (%sse-protocol-error
     "CORS credentials must be NIL or T."
     value))
  (when (and value
             (or (null origin)
                 (string= origin "*")))
    (%sse-protocol-error
     "CORS credentials require a non-wildcard origin."
     origin))
  value)

(defun %sse-http-x-accel-buffering-value (value)
  (cond
    ((null value)
     nil)
    ((eq value t)
     "no")
    ((stringp value)
     (let ((normalized (string-trim '(#\Space #\Tab) value)))
       (cond
         ((string-equal normalized "no") "no")
         ((string-equal normalized "yes") "yes")
         (t
          (%sse-protocol-error
           "X-Accel-Buffering must be NIL, T, YES, or NO."
           value)))))
    (t
     (%sse-protocol-error
      "X-Accel-Buffering must be NIL, T, YES, or NO."
      value))))

(defun %sse-http-response-headers
    (headers cors-origin cors-credentials-p x-accel-buffering)
  (let* ((user-headers (%sse-http-user-headers headers))
         (origin (%sse-http-cors-origin cors-origin))
         (credentials (%sse-http-cors-credentials-p
                       cors-credentials-p origin))
         (buffering (%sse-http-x-accel-buffering-value
                     x-accel-buffering)))
    (append
     (list (http-message-kit:make-http-header
            "content-type" "text/event-stream; charset=utf-8")
           (http-message-kit:make-http-header
            "cache-control" "no-cache"))
     (when origin
       (list (http-message-kit:make-http-header
              "access-control-allow-origin" origin)))
     (when (and origin (not (string= origin "*")))
       (list (http-message-kit:make-http-header
              "vary" "Origin")))
     (when credentials
       (list (http-message-kit:make-http-header
              "access-control-allow-credentials" "true")))
     (when buffering
       (list (http-message-kit:make-http-header
              "x-accel-buffering" buffering)))
     user-headers)))

(defun %sse-http-check-success-status (status)
  (unless (and (integerp status)
               (= status 200))
    (%sse-protocol-error
     "An HTTP SSE response must use status 200."
     status))
  status)

(defun make-http-sse-response
    (&key (status 200) reason headers cors-origin cors-credentials-p
          x-accel-buffering trailers body (protocol-version "HTTP/1.1"))
  "Return an HTTP-MESSAGE-KIT response with SSE headers."
  (%sse-http-check-success-status status)
  (http-message-kit:make-http-response
   :protocol-version protocol-version
   :status status
   :reason reason
   :headers (%sse-http-response-headers
             headers cors-origin cors-credentials-p x-accel-buffering)
   :trailers trailers
   :body body))

(defun make-http-sse-response-stream
    (&key (status 200) reason headers cors-origin cors-credentials-p
          x-accel-buffering trailers body-function body-length
          (protocol-version "HTTP/1.1"))
  "Return an HTTP-MESSAGE-KIT response stream with SSE headers."
  (%sse-http-check-success-status status)
  (http-message-kit:make-http-response-stream
   :protocol-version protocol-version
   :status status
   :reason reason
   :headers (%sse-http-response-headers
             headers cors-origin cors-credentials-p x-accel-buffering)
   :trailers trailers
   :body-function body-function
   :body-length body-length))

(defun %sse-http-quality-value-valid-p (value)
  (and (stringp value)
       (let ((length (length value)))
         (or (string= value "0")
             (string= value "1")
             (and (<= 2 length 5)
                  (member (char value 0) '(#\0 #\1) :test #'char=)
                  (char= (char value 1) #\.)
                  (every (lambda (character)
                           (<= (char-code #\0)
                               (char-code character)
                               (char-code #\9)))
                         (subseq value 2))
                  (or (char= (char value 0) #\0)
                      (every (lambda (character)
                               (char= character #\0))
                             (subseq value 2))))))))

(defun %sse-http-quality-positive-p (value)
  (and (%sse-http-quality-value-valid-p value)
       (or (char= (char value 0) #\1)
           (and (> (length value) 2)
                (some (lambda (character)
                        (not (char= character #\0)))
                      (subseq value 2))))))

(defun %sse-http-accept-parameter-valid-p (parameter)
  (let* ((equals (position #\= parameter))
         (name (and equals (subseq parameter 0 equals)))
         (raw-value (and equals (subseq parameter (1+ equals))))
         (quality-p (and name (string-equal name "q"))))
    (multiple-value-bind (decoded-value value-valid-p)
        (%sse-http-parameter-value raw-value)
      (values
       (and (plusp (length parameter))
            equals
            (%sse-http-token-p name)
            value-valid-p
            (or (not quality-p)
                (and (%sse-http-token-p raw-value)
                     (%sse-http-quality-value-valid-p decoded-value))))
       quality-p
       decoded-value))))

(defun %sse-http-accept-media-range-specificity (media-range)
  (let ((slash (position #\/ media-range)))
    (if (or (null slash)
            (zerop slash)
            (= slash (1- (length media-range)))
            (position #\/ media-range :start (1+ slash)))
        (values nil nil)
        (let ((type (subseq media-range 0 slash))
              (subtype (subseq media-range (1+ slash))))
          (cond
            ((and (string= type "*") (string= subtype "*"))
             (values 0 t))
            ((string= type "*")
             (values nil nil))
            ((not (%sse-http-token-p type))
             (values nil nil))
            ((string= subtype "*")
             (values (and (string-equal type "text") 1) t))
            ((not (%sse-http-token-p subtype))
             (values nil nil))
            ((and (string-equal type "text")
                  (string-equal subtype "event-stream"))
             (values 2 t))
            (t
             (values nil t)))))))

(defun %sse-http-accept-entry-supports-sse-p (entry)
  (let ((end (length entry)))
    (multiple-value-bind (parameter-separator syntax-valid-p)
        (%sse-http-value-separator entry 0 end #\;)
      (unless syntax-valid-p
        (return-from %sse-http-accept-entry-supports-sse-p
          (values nil nil nil)))
      (let ((media-range
              (string-trim
               '(#\Space #\Tab)
               (subseq entry 0 parameter-separator))))
        (unless (plusp (length media-range))
          (return-from %sse-http-accept-entry-supports-sse-p
            (values nil nil nil)))
        (multiple-value-bind (specificity media-valid-p)
            (%sse-http-accept-media-range-specificity media-range)
          (unless media-valid-p
            (return-from %sse-http-accept-entry-supports-sse-p
              (values nil nil nil)))
          (if (= parameter-separator end)
              (values specificity t t)
              (let ((current (1+ parameter-separator))
                    (quality-positive-p t)
                    (quality-seen-p nil))
                (loop
                  (multiple-value-bind (separator parameter-syntax-valid-p)
                      (%sse-http-value-separator entry current end #\;)
                    (unless parameter-syntax-valid-p
                      (return-from %sse-http-accept-entry-supports-sse-p
                        (values nil nil nil)))
                    (let ((parameter
                            (string-trim
                             '(#\Space #\Tab)
                             (subseq entry current separator))))
                      (multiple-value-bind
                            (valid-p quality-p quality-value)
                          (%sse-http-accept-parameter-valid-p parameter)
                        (unless valid-p
                          (return-from %sse-http-accept-entry-supports-sse-p
                            (values nil nil nil)))
                        (when quality-p
                          (when quality-seen-p
                            (return-from %sse-http-accept-entry-supports-sse-p
                              (values nil nil nil)))
                          (setf quality-seen-p t
                                quality-positive-p
                                (%sse-http-quality-positive-p quality-value))))
                    (if (= separator end)
                        (return
                          (values specificity quality-positive-p t))
                        (setf current (1+ separator)))))))))))))

(defun %sse-http-accept-value-supports-sse-p (value)
  (when (%sse-http-header-value-p value)
    (let ((start 0)
          (end (length value))
          (best-specificity nil)
          (best-quality-positive-p nil))
      (loop
        (multiple-value-bind (comma syntax-valid-p)
            (%sse-http-value-separator value start end #\,)
          (unless syntax-valid-p
            (return (values nil nil nil)))
          (let ((entry
                  (string-trim
                   '(#\Space #\Tab)
                   (subseq value start comma))))
            (when (zerop (length entry))
              (return (values nil nil nil)))
            (multiple-value-bind
                  (entry-specificity entry-quality-positive-p entry-valid-p)
                (%sse-http-accept-entry-supports-sse-p entry)
              (unless entry-valid-p
                (return (values nil nil nil)))
              (when (and entry-specificity
                         (or (null best-specificity)
                             (> entry-specificity best-specificity)))
                (setf best-specificity entry-specificity
                      best-quality-positive-p entry-quality-positive-p))))
          (if (= comma end)
              (return
                (values best-specificity best-quality-positive-p t))
              (setf start (1+ comma))))))))

(defun %sse-http-accept-values-valid-and-supported-p (accept-values)
  (if (null accept-values)
      t
      (let ((valid-p t)
            (best-specificity nil)
            (best-quality-positive-p nil))
        (dolist (value accept-values)
          (multiple-value-bind
                (value-specificity value-quality-positive-p value-valid-p)
               (%sse-http-accept-value-supports-sse-p value)
            (setf valid-p (and valid-p value-valid-p))
            (when (and value-specificity
                       (or (null best-specificity)
                           (> value-specificity best-specificity)))
              (setf best-specificity value-specificity
                    best-quality-positive-p value-quality-positive-p))))
        (and valid-p best-specificity best-quality-positive-p))))

(defun %sse-http-last-event-id-valid-p (value)
  (and (%sse-http-header-value-p value)
       (not (find #\Null value :test #'char=))
       (%sse-no-line-breaks-p value)))

(defun %sse-http-request-fields (request)
  (handler-case
      (values (http-message-kit:http-request-method request)
              (http-message-kit:http-request-headers request)
              (http-message-kit:http-request-body request)
              t)
    (error ()
      (values nil nil nil nil))))

(defun %sse-http-request-header-values (headers name)
  (handler-case
      (let ((values (http-message-kit:http-header-values headers name)))
        (if (listp values)
            (values values t)
            (values nil nil)))
    (error ()
      (values nil nil))))

(defun http-sse-request-valid-p (request)
  "Return true when REQUEST has the required SSE request metadata."
  (and (http-message-kit:http-request-p request)
       (multiple-value-bind (method headers body fields-valid-p)
           (%sse-http-request-fields request)
         (and fields-valid-p
              (stringp method)
              (string= method "GET")
              (or (null body)
                  (and (%sse-octet-vector-p body)
                       (zerop (length body))))
              (multiple-value-bind (accept-values accept-valid-p)
                  (%sse-http-request-header-values headers "accept")
                (and accept-valid-p
                     (%sse-http-accept-values-valid-and-supported-p
                      accept-values)
                     (multiple-value-bind (last-event-ids last-valid-p)
                         (%sse-http-request-header-values
                          headers "last-event-id")
                       (and last-valid-p
                            (<= (length last-event-ids) 1)
                            (every #'%sse-http-last-event-id-valid-p
                                   last-event-ids)))))))))

(defun %sse-http-request-error (status message detail &optional headers)
  (error 'sse-http-error
         :message message
         :operation :http-request
         :detail detail
         :status status
         :headers headers))

(defun validate-http-sse-request (request)
  "Validate REQUEST or signal an HTTP-level SSE condition."
  (unless (http-message-kit:http-request-p request)
    (%sse-protocol-error
     "SSE request validation requires an HTTP request."
     request))
  (multiple-value-bind (method headers body fields-valid-p)
      (%sse-http-request-fields request)
    (unless fields-valid-p
      (%sse-http-request-error
       400
       "The HTTP request contains malformed metadata."
       nil))
    (multiple-value-bind (accept-values accept-valid-p)
        (%sse-http-request-header-values headers "accept")
      (multiple-value-bind (last-event-ids last-valid-p)
          (%sse-http-request-header-values headers "last-event-id")
        (cond
          ((not (and (stringp method) (string= method "GET")))
           (%sse-http-request-error
            (if (stringp method) 405 400)
            "SSE requests must use GET."
            (list :method method)
            (when (and (stringp method) (not (string= method "GET")))
              (list (http-message-kit:make-http-header "allow" "GET")))))
          ((not (or (null body) (%sse-octet-vector-p body)))
           (%sse-http-request-error
            400
            "SSE requests must contain an octet-vector body or no body."
            nil))
          ((and body (plusp (length body)))
           (%sse-http-request-error
            400
            "SSE requests must not contain a request body."
            (list :body-length (length body))))
          ((not accept-valid-p)
           (%sse-http-request-error
            400
            "The Accept header metadata is malformed."
            nil))
          ((not (%sse-http-accept-values-valid-and-supported-p accept-values))
           (%sse-http-request-error
            406
            "The request does not accept text/event-stream."
            (list :accept accept-values)))
          ((not last-valid-p)
           (%sse-http-request-error
            400
            "The Last-Event-ID header metadata is malformed."
            nil))
          ((> (length last-event-ids) 1)
           (%sse-http-request-error
            400
            "SSE requests must contain at most one Last-Event-ID header."
            (list :last-event-id last-event-ids)))
          ((some (lambda (value)
                   (not (%sse-http-last-event-id-valid-p value)))
                 last-event-ids)
           (%sse-http-request-error
            400
            "Last-Event-ID contains a forbidden character."
            (list :last-event-id last-event-ids)))
          (t
           request))))))

(defun http-sse-request-last-event-id (request)
  "Return REQUEST's UTF-8 decoded Last-Event-ID, or the empty string."
  (validate-http-sse-request request)
  (let ((value (%sse-http-single-header-value
                (http-message-kit:http-request-headers request)
                "last-event-id")))
    (if value
        (%sse-http-header-value-to-utf8 value)
        "")))
