(in-package #:asdf-user)

(asdf:defsystem "cl-sse-kit"
  :description "HTTP Server-Sent Events parser, serializer, session, publisher, and client protocol state."
  :author "nerima-lisp"
  :license "MIT"
  :version "1.1.0"
  :depends-on ("cl-codec-kit"
               "cl-http-message-kit"
               "cl-http-kit"
               "cl-resilience-kit")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "data")
               (:file "octets")
               (:file "parser")
               (:file "parser-io")
               (:file "serialize")
               (:file "http")
               (:file "session")
               (:file "publisher")
               (:file "client"))
  :in-order-to ((test-op (test-op "cl-sse-kit/test"))))

(asdf:defsystem "cl-sse-kit/test"
  :description "Tests for cl-sse-kit."
  :depends-on ("cl-sse-kit"
               "cl-codec-kit"
               "cl-http-message-kit"
               "cl-http-kit"
               "cl-http-kit/client"
               "cl-http-kit/network"
               "cl-http-kit/tls"
               "cl-resilience-kit"
               "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "tests-parse")
               (:file "tests-serialize")
               (:file "tests-world")
               (:file "tests-e2e-https")
               (:file "runner"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call "SSE-KIT/TEST" "RUN-TESTS")))
