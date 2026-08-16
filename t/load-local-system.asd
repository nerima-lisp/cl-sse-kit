(require :asdf)

(let ((clear (find-symbol "CLEAR-SYSTEM" :asdf))
      (register (find-symbol "REGISTER-IMMUTABLE-SYSTEM" :asdf)))
  (when (and clear (fboundp clear))
    (ignore-errors (funcall clear "cl-weave")))
  (when (and register (fboundp register))
    (funcall register "cl-weave")))

(let ((pathname (or *load-truename* *load-pathname*)))
  (load (merge-pathnames "../cl-sse-kit.asd" pathname)))
