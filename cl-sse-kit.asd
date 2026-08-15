(in-package #:asdf-user)

(asdf:defsystem "cl-sse-kit"
  :description "Server-Sent Events (text/event-stream) parser and serializer."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "data")
               (:file "octets")
               (:file "parser")
               (:file "serialize"))
  :in-order-to ((test-op (test-op "cl-sse-kit/test"))))

(asdf:defsystem "cl-sse-kit/test"
  :description "Tests for cl-sse-kit."
  :depends-on ("cl-sse-kit" "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "tests-parse")
               (:file "tests-serialize")
               (:file "runner"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call "SSE-KIT/TEST" "RUN-TESTS")))
