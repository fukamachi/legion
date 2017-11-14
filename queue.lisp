(defpackage #:legion/queue
  (:use #:cl)
  (:import-from #:cl-speedy-queue)
  (:import-from #:bordeaux-threads)
  (:export #:queue
           #:make-queue
           #:enqueue
           #:dequeue
           #:queue-empty-p
           #:queue-count))
(in-package #:legion/queue)

(defstruct (queue (:constructor %make-queue))
  primary
  (buffered '())
  (lock (bt:make-lock "queue lock")))

(defun make-queue (&optional (initial-size 128))
  (%make-queue :primary (cl-speedy-queue:make-queue initial-size)))

(defun queue-to-enqueue (queue)
  (or (first (queue-buffered queue))
      (queue-primary queue)))

(defun need-to-extend-p (queue)
  (and (cl-speedy-queue:queue-full-p (queue-primary queue))
       (let ((next-queue (first (queue-buffered queue))))
         (or (null next-queue)
             (cl-speedy-queue:queue-full-p next-queue)))))

(defun extend (queue)
  (let* ((next-size (* 2 (cl-speedy-queue:queue-length
                          (or (first (queue-buffered queue))
                              (queue-primary queue)))))
         (new-queue (cl-speedy-queue:make-queue next-size)))
    (push new-queue
          (queue-buffered queue))
    new-queue))

(defun need-to-tidy-p (queue)
  (and (queue-buffered queue)
       (cl-speedy-queue:queue-empty-p (queue-primary queue))))

(defun tidy (queue)
  (when (null (cdr (queue-buffered queue)))
    (let ((last (first (queue-buffered queue))))
      (setf (queue-primary queue) last
            (queue-buffered queue) nil)
      (return-from tidy last)))
  (loop for xs on (queue-buffered queue)
        if (null (cddr xs))
          do (let ((last (second xs)))
               (rplacd xs nil)
               (setf (queue-primary queue) last)
               (return last))))

(defun enqueue (job queue)
  (bt:with-lock-held ((queue-lock queue))
    (cl-speedy-queue:enqueue
     job
     (if (need-to-extend-p queue)
         (extend queue)
         (queue-to-enqueue queue)))))

(defun dequeue (queue)
  (bt:with-lock-held ((queue-lock queue))
    (prog1
        (cl-speedy-queue:dequeue (queue-primary queue))
      (when (need-to-tidy-p queue)
        (tidy queue)))))

(defun queue-empty-p (queue)
  (cl-speedy-queue:queue-empty-p
   (queue-primary queue)))

(defun queue-count (queue)
  (reduce #'+
          (cons (queue-primary queue)
                (queue-buffered queue))
          :key #'cl-speedy-queue:queue-count))
