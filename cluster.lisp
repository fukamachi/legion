(defpackage #:legion/cluster
  (:use #:cl)
  (:import-from #:legion/worker
                #:worker
                #:worker-status
                #:worker-queue
                #:worker-thread
                #:start
                #:stop
                #:kill
                #:add-job
                #:wakenup)
  (:import-from #:legion/queue
                #:make-queue
                #:enqueue
                #:queue-empty-p)
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

(defclass cluster ()
  ((status :initform :shutdown
           :accessor cluster-status)
   (queue :initarg :queue
          :initform (make-queue)
          :accessor cluster-queue)

   (workers :initarg :workers
            :initform '()
            :reader cluster-workers)))

(defmethod initialize-instance :after ((cluster cluster) &rest initargs &key workers &allow-other-keys)
  (declare (ignore initargs))
  (mapc (lambda (worker)
          (unless (queue-empty-p (worker-queue worker))
            (error "Worker queue is not empty"))
          (setf (worker-queue worker)
                (cluster-queue cluster)))
        workers))

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
  (enqueue job (cluster-queue cluster))
  (let ((idle-worker (find :idle (cluster-workers cluster) :key #'worker-status)))
    (when idle-worker
      (wakenup idle-worker)))
  cluster)
