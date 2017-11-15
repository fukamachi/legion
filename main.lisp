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

           #:cluster
           #:cluster-status
           #:cluster-workers

           #:legion-error))
(in-package #:legion)
