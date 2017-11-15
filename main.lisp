(uiop:define-package #:legion
  (:nicknames #:legion/main)
  (:use #:cl)
  (:import-from #:legion/worker
                #:worker
                #:make-worker
                #:worker-status
                #:worker-queue-count
                #:start
                #:stop
                #:kill
                #:add-job
                #:next-job)
  (:import-from #:legion/cluster
                #:cluster
                #:make-cluster
                #:cluster-status
                #:cluster-workers)
  (:import-from #:legion/error
                #:legion-error)
  (:export #:worker
           #:make-worker
           #:worker-status
           #:worker-queue-count
           #:start
           #:stop
           #:kill
           #:add-job
           #:next-job

           #:cluster
           #:make-cluster
           #:cluster-status
           #:cluster-workers

           #:legion-error))
(in-package #:legion)
