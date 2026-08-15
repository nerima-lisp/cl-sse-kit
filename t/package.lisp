(defpackage #:sse-kit/test
  (:use #:cl #:sse-kit)
  (:shadowing-import-from #:cl-weave
                          #:describe)
  (:import-from #:cl-weave
                #:expect
                #:it
                #:run-all
                #:signals)
  (:export #:run-tests))

(in-package #:sse-kit/test)

(defun lf (&rest lines)
  "Join LINES with a line feed, including a trailing one."
  (format nil "~{~A~%~}" lines))

(defun crlf (&rest lines)
  "Join LINES with CRLF, including a trailing one."
  (with-output-to-string (out)
    (dolist (line lines)
      (format out "~A~C~C" line #\Return #\Linefeed))))

(defun octets-string (octets)
  (map 'string #'code-char octets))
