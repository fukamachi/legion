#|
  This file is a part of simple-worker project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage simple-worker-test-asd
  (:use :cl :asdf))
(in-package :simple-worker-test-asd)

(defsystem simple-worker-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:simple-worker
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "simple-worker"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
