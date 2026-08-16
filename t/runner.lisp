(in-package #:sse-kit/test)

(defun run-tests ()
  (unless (run-all :reporter :spec
                   :max-workers 1
                   :timeout-ms 300000
                   :pass-with-no-tests nil)
    (error "cl-sse-kit tests failed."))
  t)
