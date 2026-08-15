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
