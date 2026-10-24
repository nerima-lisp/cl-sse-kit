(in-package #:sse-kit/http-kit-client/test)

(defun run-tests ()
  (unless (run-all :reporter :spec
                   :max-workers 1
                   :timeout-ms 300000
                   :pass-with-no-tests nil)
    (error "cl-sse-kit HTTP client integration tests failed."))
  t)
