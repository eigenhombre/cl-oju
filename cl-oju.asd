(defsystem :cl-oju
  :version "0.0.1"
  :author "John Jacobsen"
  :license "MIT"
  :depends-on (:1am)
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op :cl-oju/test)))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "main"))))
  :description "Common Lisp equivalents of core Clojure functions, especially sequence-related ones")

(defsystem :cl-oju/test
  :author "John Jacobsen"
  :version "0.0.1"
  :license "MIT"
  :depends-on (:cl-oju :1am)
  :description "Tests for cl-oju"
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "main"))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string "cl-oju-tests:run-tests"))))
