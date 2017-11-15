(defpackage #:legion/scheduler
  (:use #:cl)
  (:import-from #:legion/worker
                #:add-job)
  (:export #:make-round-robin-scheduler))
(in-package #:legion/scheduler)

(defun make-round-robin-scheduler (workers)
  (let ((workers (copy-seq workers)))
    (setf (cdr (last workers)) workers)
    (lambda (job)
      (add-job (pop workers) job))))
