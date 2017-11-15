(defpackage #:legion/cluster
  (:use #:cl)
  (:import-from #:legion/worker
                #:worker
                #:worker-status
                #:make-worker
                #:start
                #:stop
                #:kill
                #:add-job
                #:worker-thread)
  (:import-from #:legion/scheduler
                #:make-round-robin-scheduler)
  (:import-from #:legion/error
                #:legion-error)
  (:import-from #:bordeaux-threads
                #:join-thread)
  (:export #:cluster
           #:make-cluster
           #:cluster-status
           #:cluster-workers
           #:start
           #:stop
           #:kill
           #:add-job))
(in-package #:legion/cluster)

(defun make-workers-array (worker-num process-fn queue)
  (let ((workers (make-array worker-num :element-type 'worker)))
    (dotimes (i worker-num workers)
      (setf (svref workers i)
            (make-worker process-fn
                         :queue queue)))))

(defstruct (cluster (:constructor make-cluster
                        (worker-num process-fn &key queue scheduler
                         &aux
                           (workers (make-workers-array worker-num process-fn queue))
                           (scheduler (or scheduler
                                          (make-round-robin-scheduler workers))))))
  (status :shutdown)
  workers
  scheduler)

(defmethod print-object ((object cluster) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":STATUS ~A :WORKER-NUM ~A"
            (cluster-status object)
            (length (cluster-workers object)))))

(defmethod start ((cluster cluster))
  (loop for worker across (cluster-workers cluster)
        do (start worker))
  (setf (cluster-status cluster) :running)
  (vom:info "cluster has started.")
  cluster)

(defmethod stop ((cluster cluster))
  (loop for worker across (cluster-workers cluster)
        do (stop worker))
  (setf (cluster-status cluster) :shutting)
  (loop for worker across (cluster-workers cluster)
        for thread = (worker-thread worker)
        when thread
          do (join-thread thread))
  (setf (cluster-status cluster) :shutdown)
  cluster)

(defmethod kill ((cluster cluster))
  (loop for worker across (cluster-workers cluster)
        do (kill worker))
  (setf (cluster-status cluster) :shutdown)
  cluster)

(defmethod add-job ((cluster cluster) job)
  (when (eq (cluster-status cluster) :shutting)
    (return-from add-job nil))
  (let ((workers (cluster-workers cluster)))
    (unless (funcall (cluster-scheduler cluster)
                     workers
                     job)
      (error "Failed to add a job"))
    t))
