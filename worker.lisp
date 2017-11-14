(defpackage #:legion/worker
  (:use #:cl)
  (:import-from #:cl-speedy-queue
                #:make-queue
                #:enqueue
                #:dequeue
                #:queue-count
                #:queue-empty-p
                #:queue-full-p)
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
           #:make-worker
           #:worker-status
           #:worker-queue-count
           #:worker-idle-cond
           #:start-worker
           #:stop-worker
           #:kill-worker
           #:add-job-to-worker
           #:next-job))
(in-package #:legion/worker)

(defstruct (worker (:constructor make-worker (process-fn &key (queue-size 128)
                                              &aux (queue (make-queue queue-size)))))
  (status :shutdown)
  thread
  queue
  process-fn
  (queue-lock (make-recursive-lock "queue-lock"))
  (wait-lock (make-recursive-lock "wait-lock"))
  (wait-cond (make-condition-variable))
  (idle-cond (make-condition-variable)))

(defmethod print-object ((object worker) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":STATUS ~A :QUEUE-COUNT ~A"
            (worker-status object)
            (worker-queue-count object))))

(defun make-thread-function (worker)
  (let ((process-fn (worker-process-fn worker))
        (queue (worker-queue worker))
        (wait-lock (worker-wait-lock worker))
        (wait-cond (worker-wait-cond worker)))
    (lambda ()
      (unwind-protect
           (loop
             (when (queue-empty-p queue)
               (when (eq (worker-status worker) :shutting)
                 (return))
               (setf (worker-status worker) :idle)
               (condition-notify (worker-idle-cond worker))
               (with-recursive-lock-held (wait-lock)
                 (condition-wait wait-cond wait-lock)))
             (funcall process-fn worker))
        (vom:info "worker is shutting down. bye.")
        (setf (worker-status worker) :shutdown)))))

(defun worker-queue-count (worker)
  "Return the number of outstanding jobs."
  (queue-count (worker-queue worker)))

(defun start-worker (worker)
  "Start the given WORKER.
It raises an error if the WORKER is already running."
  (with-slots (thread status) worker
    (when thread
      (error "Worker is already running."))
    (setf status :running)
    (setf thread
          (make-thread (make-thread-function worker)
                       :name "legion")))
  (vom:info "worker has started.")
  worker)

(defun stop-worker (worker)
  "Stop the given WORKER after processing its queued jobs.
It raises an error if the WORKER is not running."
  (with-slots (thread status) worker
    (unless thread
      (error "Worker is not running."))
    (if (eq status :idle)
        (kill-worker worker)
        (progn
          (setf status :shutting)
          (vom:info "worker is going to be shutted down."))))
  worker)

(defun kill-worker (worker)
  "Stop the given WORKER immediately.
It raises an error if the WORKER is not running."
  (with-slots (thread status) worker
    (unless thread
      (error "Worker is not running"))
    (when (thread-alive-p thread)
      (destroy-thread thread))
    (vom:info "worker has been killed.")
    (setf thread nil
          status :shutdown))
  worker)

(defun add-job-to-worker (worker val)
  "Enqueue VAL to WORKER's queue. This returns WORKER when the queueing has been succeeded; otherwise NIL is returned."
  (with-slots (status queue queue-lock wait-cond) worker
    (when (or (eq status :shutting)
              (queue-full-p queue))
      (return-from add-job-to-worker nil))
    (with-recursive-lock-held (queue-lock)
      (enqueue val queue))
    (when (eq status :idle)
      (condition-notify wait-cond)
      (setf status :running)))
  worker)

(defun next-job (worker)
  "Dequeue a value from WORKER's queue. This returns multiple values -- the job and a successed flag."
  (with-slots (queue queue-lock) worker
    (if (queue-empty-p queue)
        (values nil nil)
        (values (with-recursive-lock-held (queue-lock)
                  (dequeue queue))
                t))))
