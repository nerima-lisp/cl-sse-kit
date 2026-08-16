(in-package #:sse-kit)

;; The slot shape (message, operation, detail) mirrors the protocol-error
;; conditions used by the HTTP libraries that consume this parser, so a caller
;; can translate one into the other without losing the diagnostic payload.
(define-condition sse-error (error)
  ((message :initarg :message :reader sse-error-message)
   (operation :initarg :operation :initform nil :reader sse-error-operation)
   (detail :initarg :detail :initform nil :reader sse-error-detail))
  (:report (lambda (condition stream)
             (format stream "~A" (sse-error-message condition)))))

;; Signalled when a stream exceeds one of the caller-supplied budgets. The
;; limit, what was actually observed, and which budget it was are separate
;; slots so a caller can decide whether to raise the budget or drop the
;; stream, rather than parsing a message string.
(define-condition sse-size-limit-exceeded (sse-error)
  ((limit :initarg :limit :reader sse-size-limit-exceeded-limit)
   (observed :initarg :observed :reader sse-size-limit-exceeded-observed)
   (kind :initarg :kind :reader sse-size-limit-exceeded-kind)))

(define-condition sse-http-error (sse-error)
  ((status :initarg :status :reader sse-http-error-status)
   (headers :initarg :headers :initform nil :reader sse-http-error-headers)))

(define-condition sse-replay-unavailable (sse-error)
  ((last-event-id :initarg :last-event-id
                  :reader sse-replay-unavailable-last-event-id)))

(define-condition sse-client-disconnected (sse-error) ())

(defun %sse-call/k (thunk on-success on-error)
  (unless (functionp thunk)
    (error 'type-error :datum thunk :expected-type 'function))
  (unless (functionp on-success)
    (error 'type-error :datum on-success :expected-type 'function))
  (unless (functionp on-error)
    (error 'type-error :datum on-error :expected-type 'function))
  (let (values failed)
    (handler-case
        (setf values (multiple-value-list (funcall thunk)))
      (error (condition)
        (setf failed t)
        (funcall on-error condition)))
    (unless failed
      (apply on-success values))))
