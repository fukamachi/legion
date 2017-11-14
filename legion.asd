(defsystem "legion"
  :class :package-inferred-system
  :version "0.1.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("vom"
               "legion/main")
  :description "Simple worker threads with a queue."
  :in-order-to ((test-op (test-op "legion-test"))))
