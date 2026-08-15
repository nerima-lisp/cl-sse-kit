(in-package #:sse-kit/test)

(defun run-tests ()
  (unless (run-all :reporter :spec :pass-with-no-tests nil)
    (error "cl-sse-kit tests failed."))
  t)
