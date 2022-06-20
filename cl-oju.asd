(defsystem :cl-oju
  :version "0.0.1"
  :author "John Jacobsen"
  :license "TBD"
  :depends-on (:let-plus :1am)
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op :cl-oju/test)))
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Some Clojure-ish thingies")

(defsystem :cl-oju/test
  :depends-on (:cl-oju :1am)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "main"))))
  :perform (asdf:test-op (op system)
                         (funcall (read-from-string "cl-oju-tests:run-tests"))))
