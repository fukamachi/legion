(uiop:define-package #:legion
  (:nicknames #:legion/main)
  (:use #:cl)
  (:import-from #:legion/worker
                #:worker
                #:make-worker
                #:worker-status
                #:worker-queue-count
                #:start-worker
                #:stop-worker
                #:kill-worker
                #:add-job-to-worker
                #:next-job)
  (:import-from #:legion/cluster
                #:cluster
                #:make-cluster
                #:cluster-status
                #:cluster-workers
                #:start-cluster
                #:stop-cluster
                #:kill-cluster
                #:add-job-to-cluster)
  (:import-from #:legion/error
                #:legion-error)
  (:export #:worker
           #:make-worker
           #:worker-status
           #:worker-queue-count
           #:start-worker
           #:stop-worker
           #:kill-worker
           #:add-job
           #:next-job

           #:cluster
           #:make-cluster
           #:cluster-status
           #:cluster-workers
           #:start-cluster
           #:stop-cluster
           #:kill-cluster

           #:legion-error))
(in-package #:legion)

(defun add-job (to job)
  (etypecase to
    (cluster (add-job-to-cluster to job))
    (worker (add-job-to-worker to job))))
