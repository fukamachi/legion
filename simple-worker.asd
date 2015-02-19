#|
  This file is a part of simple-worker project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage simple-worker-asd
  (:use :cl :asdf))
(in-package :simple-worker-asd)

(defsystem simple-worker
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:cl-speedy-queue
               :bordeaux-threads
               :vom)
  :components ((:module "src"
                :components
                ((:file "simple-worker"))))
  :description "Simple worker threads with a queue."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op simple-worker-test))))
