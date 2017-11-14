(defpackage #:legion/error
  (:use #:cl)
  (:export #:legion-error))
(in-package #:legion/error)

(define-condition legion-error (error) ())
