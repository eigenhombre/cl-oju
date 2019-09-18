(defsystem "cl-oju"
  :version "0.0.1"
  :author "John Jacobsen"
  :license "TBD"
  :depends-on ("trivialtests")
  :components ((:module "src"
                        :components
                        ((:file "main"))))
  :description "Some Clojure-ish thingies")
