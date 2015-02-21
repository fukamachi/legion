(in-package :cl-user)
(defpackage legion
  (:use :cl)
  (:import-from :legion.worker
                :worker
                :make-worker
                :worker-status
                :worker-queue-count
                :start-worker
                :stop-worker
                :kill-worker
                :add-job-to-worker
                :next-job)
  (:import-from :legion.cluster
                :cluster
                :make-cluster
                :cluster-status
                :cluster-workers
                :start-cluster
                :stop-cluster
                :kill-cluster
                :add-job-to-cluster)
  (:export :worker
           :make-worker
           :worker-status
           :worker-queue-count
           :start-worker
           :stop-worker
           :kill-worker
           :add-job
           :next-job

           :cluster
           :make-cluster
           :cluster-status
           :cluster-workers
           :start-cluster
           :stop-cluster
           :kill-cluster))
(in-package :legion)

(defun add-job (to job)
  (etypecase to
    (cluster (add-job-to-cluster to job))
    (worker (add-job-to-worker to job))))
