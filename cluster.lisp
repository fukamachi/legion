(defpackage #:legion/cluster
  (:use #:cl)
  (:import-from #:legion/worker
                #:worker
                #:worker-status
                #:start
                #:stop
                #:kill
                #:add-job
                #:worker-thread)
  (:import-from #:legion/queue
                #:make-queue)
  (:import-from #:legion/scheduler
                #:make-round-robin-scheduler)
  (:import-from #:legion/error
                #:legion-error)
  (:import-from #:bordeaux-threads
                #:join-thread)
  (:export #:cluster
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
            (make-instance 'worker
                           :process-fn process-fn
                           :queue queue)))))

(defclass cluster ()
  ((status :initform :shutdown
           :accessor cluster-status)
   (scheduler :initarg :scheduler
              :accessor cluster-scheduler)

   (workers :initarg :workers
            :initform '()
            :reader cluster-workers)))

(defmethod initialize-instance :after ((cluster cluster) &key workers &allow-other-keys)
  (unless (slot-boundp cluster 'scheduler)
    (setf (slot-value cluster 'scheduler)
          (make-round-robin-scheduler workers))))

(defmethod print-object ((object cluster) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":STATUS ~A :WORKER-NUM ~A"
            (cluster-status object)
            (length (cluster-workers object)))))

(defmethod start ((cluster cluster))
  (mapc #'start (cluster-workers cluster))
  (setf (cluster-status cluster) :running)
  (vom:info "cluster has started.")
  cluster)

(defmethod stop ((cluster cluster))
  (mapc #'stop (cluster-workers cluster))
  (setf (cluster-status cluster) :shutting)
  (mapc (lambda (worker)
          (let ((thread (worker-thread worker)))
            (when thread
              (join-thread thread))))
        (cluster-workers cluster))
  (setf (cluster-status cluster) :shutdown)
  cluster)

(defmethod kill ((cluster cluster))
  (mapc #'kill (cluster-workers cluster))
  (setf (cluster-status cluster) :shutdown)
  cluster)

(defmethod add-job ((cluster cluster) job)
  (when (eq (cluster-status cluster) :shutting)
    (return-from add-job nil))
  (unless (funcall (cluster-scheduler cluster) job)
    (error "Failed to add a job"))
  t)
