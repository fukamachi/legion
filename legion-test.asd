#|
  This file is a part of legion project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage legion-test-asd
  (:use :cl :asdf))
(in-package :legion-test-asd)

(defsystem legion-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:legion
               :local-time
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "legion"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
