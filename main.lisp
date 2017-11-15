(uiop:define-package #:legion
  (:nicknames #:legion/main)
  (:use #:cl)
  (:import-from #:legion/worker
                #:worker
                #:worker-status
                #:worker-queue-count
                #:fetch-job
                #:process-job
                #:start
                #:stop
                #:kill
                #:add-job)
  (:import-from #:legion/cluster
                #:cluster
                #:cluster-status
                #:cluster-workers)
  (:import-from #:legion/error
                #:legion-error)
  (:export #:worker
           #:worker-status
           #:worker-queue-count
           #:fetch-job
           #:process-job
           #:start
           #:stop
           #:kill
           #:add-job
           #:make-worker

           #:cluster
           #:cluster-status
           #:cluster-workers
           #:make-cluster

           #:legion-error))
(in-package #:legion)

(defun make-worker (process-fn &rest initargs)
  (apply #'make-instance 'worker
         :process-fn process-fn
         initargs))

(defun make-cluster (worker-num process-fn &rest initargs)
  (apply #'make-instance 'cluster
         :workers
         (loop repeat worker-num
               collect (make-worker process-fn))
         initargs))
