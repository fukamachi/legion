(defsystem "legion-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("legion"
               "local-time"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "legion"))))

  :perform (test-op (op c) (symbol-call :prove-asdf '#:run-test-system c)))
