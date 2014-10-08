(defsystem :py-format-tests
  :depends-on ("py-format" "lisp-unit")
  :components ((:file "py-format-tests"))
  :perform (test-op (o s)
                    (uiop:symbol-call :lisp-unit 'run-tests :all :py-format-tests)))
