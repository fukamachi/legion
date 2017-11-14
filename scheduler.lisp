(defpackage #:legion/scheduler
  (:use #:cl)
  (:import-from #:legion/worker
                #:add-job-to-worker)
  (:export #:make-round-robin-scheduler))
(in-package #:legion/scheduler)

(defun make-round-robin-scheduler (workers)
  (let ((worker-num (length workers))
        (i 0))
    (lambda (workers job)
      (add-job-to-worker (svref workers (mod (incf i) worker-num))
                         job))))
