(defpackage #:legion-test
  (:use #:cl
        #:legion
        #:prove))
(in-package #:legion-test)

(plan 14)

(let ((worker (make-worker (lambda (worker)
                             (declare (ignore worker))
                             (sleep 0.3)))))
  (subtest "can make"
    (ok worker)
    (is (worker-status worker) :shutdown)
    (is (worker-queue-count worker) 0))

  (subtest "can start"
    (ok (start worker) "start-worker")
    (is (worker-status worker) :idle "status is idle")
    (is (worker-queue-count worker) 0 "queue is empty"))

  (subtest "can stop"
    (ok (stop worker) "stop-worker")
    (sleep 0.5)
    (is (worker-status worker) :shutdown "status is shutdown")
    (is (worker-queue-count worker) 0 "queue is empty")))

(let* ((bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                        (*error-output* . ,*error-output*)))
       (results (make-array 0 :adjustable t :fill-pointer 0))
       (worker (make-worker (lambda (worker)
                              (sleep 0.1)
                              (multiple-value-bind (val existsp)
                                  (next-job worker)
                                (when existsp
                                  (vector-push-extend (* val 2) results)))))))
  (subtest "can make"
    (ok worker)
    (is (worker-status worker) :shutdown)
    (is (worker-queue-count worker) 0))

  (subtest "can add-job"
    (ok (add-job worker 128) "add-job")
    (is (worker-status worker) :shutdown "status is still shutdown")
    (is (worker-queue-count worker) 1 "queue count is 1")
    (is results #() :test #'equalp))

  (subtest "can start"
    (ok (start worker) "start-worker")
    (is (worker-status worker) :running "status is running")
    (is results #() :test #'equalp))

  (sleep 0.3)

  (subtest "can process"
    (is (worker-status worker) :idle "status is idle")
    (is (worker-queue-count worker) 0 "queue is empty")
    (is results #(256) :test #'equalp)
    (dotimes (i 5)
      (add-job worker (* i 3)))
    (is (worker-status worker) :running))

  (sleep 1)

  (subtest "can stop"
    (is (worker-queue-count worker) 0 "queue is empty")
    (ok (stop worker) "stop-worker")
    (is (worker-status worker) :shutdown "status is shutdown")
    (is (worker-queue-count worker) 0 "queue is empty")
    (is results #(256 0 6 12 18 24) :test #'equalp)))

(let* ((bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                        (*error-output* . ,*error-output*)))
       (results-lock (bt:make-recursive-lock))
       (results (make-array 0 :adjustable t :fill-pointer 0))
       (cluster (make-cluster 4 (lambda (worker)
                                  (sleep 0.1)
                                  (multiple-value-bind (val existsp)
                                      (next-job worker)
                                    (when existsp
                                      (bt:with-recursive-lock-held (results-lock)
                                        (vector-push-extend (* val 2) results))))))))
  (ok cluster "can make")

  (subtest "can add-job"
    (ok (add-job cluster 128) "add-job")
    (is results #() :test #'equalp))

  (subtest "can start"
    (ok (start cluster) "start-cluster")
    (is results #() :test #'equalp))

  (sleep 0.3)

  (subtest "can process"
    (is results #(256) :test #'equalp)
    (dotimes (i 5)
      (add-job cluster (* i 3))))

  (sleep 1)

  (subtest "can stop"
    (ok (stop cluster))
    (is (cluster-status cluster) :shutdown "status is shutdown")
    (is (sort results #'<)
        #(0 6 12 18 24 256)
        :test 'equalp)))

(let* ((bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                        (*error-output* . ,*error-output*)))
       (task-count 1000)
       (worker-count 2)
       (cluster (make-cluster worker-count
                              (lambda (worker)
                                (sleep 0.01)
                                (next-job worker))))
       start)
  (subtest "process 1000 jobs"
    (dotimes (i task-count)
      (add-job cluster i))
    (start cluster)
    (setf start (local-time:now))
    (loop until (every (lambda (worker)
                         (eq (worker-status worker) :idle))
                       (cluster-workers cluster))
          do (sleep 0.1))
    (let ((took (local-time:timestamp-difference (local-time:now) start))
          (from (* (/ task-count worker-count) 0.01)))
      (ok (<= from took (* from 1.3)) (format nil "Ends in ~A-~A seconds" from (* from 1.3))))))

(finalize)
