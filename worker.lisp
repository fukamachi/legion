(defpackage #:legion/worker
  (:use #:cl)
  (:import-from #:legion/queue
                #:make-queue
                #:enqueue
                #:dequeue
                #:queue-count
                #:queue-empty-p)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:destroy-thread
                #:thread-alive-p
                #:condition-notify
                #:condition-wait
                #:make-condition-variable
                #:make-recursive-lock
                #:with-recursive-lock-held)
  (:export #:worker
           #:worker-status
           #:worker-queue-count
           #:fetch-job
           #:process-job
           #:start
           #:stop
           #:kill
           #:add-job))
(in-package #:legion/worker)

(defclass worker ()
  ((status :initform :shutdown
           :accessor worker-status)
   (queue :initarg :queue
          :initform (make-queue)
          :accessor worker-queue)

   (thread :initform nil
           :reader worker-thread)
   (wait-lock :initform (make-recursive-lock "wait-lock")
              :reader worker-wait-lock)
   (wait-cond :initform (make-condition-variable)
              :reader worker-wait-cond)))

(defun worker-queue-count (worker)
  "Return the number of outstanding jobs."
  (queue-count (worker-queue worker)))

(defmethod print-object ((object worker) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":STATUS ~A :QUEUE-COUNT ~A"
            (worker-status object)
            (worker-queue-count object))))

(defgeneric fetch-job (worker)
  (:documentation "Dequeue a value from WORKER's queue. This returns multiple values -- the job and a successed flag.")
  (:method ((worker worker))
    (dequeue (worker-queue worker))))

(defgeneric process-job (worker job))

(defgeneric run (worker)
  (:method ((worker worker))
    (let ((wait-lock (worker-wait-lock worker))
          (wait-cond (worker-wait-cond worker)))
      (loop
        (multiple-value-bind (job exists)
            (fetch-job worker)
          (if exists
              (process-job worker job)
              (progn
                (when (eq (worker-status worker) :shutting)
                  (return))
                (setf (worker-status worker) :idle)
                (with-recursive-lock-held (wait-lock)
                  (condition-wait wait-cond wait-lock))))))))
  (:method :around ((worker worker))
    (unwind-protect (call-next-method)
      (vom:info "worker is shutting down. bye.")
      (setf (worker-status worker) :shutdown))))


(defgeneric start (worker)
  (:documentation "Start the given WORKER.
It raises an error if the WORKER is already running.")
  (:method ((worker worker))
    (with-slots (thread status) worker
      (when thread
        (error "Worker is already running."))
      (setf status :running)
      (setf thread
            (make-thread (lambda () (run worker))
                         :name "legion worker")))
    (vom:info "worker has started.")
    worker))

(defgeneric stop (worker)
  (:documentation "Stop the given WORKER after processing its queued jobs.
It raises an error if the WORKER is not running.")
  (:method ((worker worker))
    (with-slots (thread status) worker
      (unless thread
        (error "Worker is not running."))
      (if (eq status :idle)
          (kill worker)
          (progn
            (setf status :shutting)
            (vom:info "worker is going to be shutted down."))))
    worker))

(defgeneric kill (worker)
  (:documentation "Stop the given WORKER immediately.
It raises an error if the WORKER is not running.")
  (:method ((worker worker))
    (with-slots (thread status) worker
      (unless thread
        (error "Worker is not running"))
      (when (thread-alive-p thread)
        (destroy-thread thread))
      (vom:info "worker has been killed.")
      (setf thread nil
            status :shutdown))
    worker))

(defgeneric add-job (worker val)
  (:documentation "Enqueue VAL to WORKER's queue. This returns WORKER when the queueing has been succeeded; otherwise NIL is returned.")
  (:method ((worker worker) val)
    (with-slots (status queue wait-cond) worker
      (when (eq status :shutting)
        (return-from add-job nil))
      (enqueue val queue)
      (when (eq status :idle)
        (condition-notify wait-cond)
        (setf status :running)))
    worker))
