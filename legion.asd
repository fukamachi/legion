(defsystem "legion"
  :version "0.1.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("cl-speedy-queue"
               "bordeaux-threads"
               "vom")
  :components ((:module "src"
                :components
                ((:file "legion" :depends-on ("worker" "cluster" "error"))
                 (:file "worker")
                 (:file "cluster" :depends-on ("worker" "scheduler" "error"))
                 (:file "scheduler" :depends-on ("worker"))
                 (:file "error"))))
  :description "Simple worker threads with a queue."
  :in-order-to ((test-op (test-op "legion-test"))))
