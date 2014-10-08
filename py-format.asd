(defsystem :py-format
    :description "Common lisp implementation of Python's PEP 3101 formatting system"
    :version "0.1.0"
    :author "Felipe Ochoa"
    :license "MIT"
    :depends-on ("closer-mop")
    :serial t
    :components ((:file "py-format-package")
                 (:file "py-format")))

(setf (asdf:component-property (asdf:find-system :py-format) :website)
      "https://github.com/felipeochoa/py-format/")
