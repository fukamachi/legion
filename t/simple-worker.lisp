(in-package :cl-user)
(defpackage simple-worker-test
  (:use :cl
        :simple-worker
        :prove))
(in-package :simple-worker-test)

(plan 8)

(let ((worker (make-worker (lambda (worker) (declare (ignore worker))))))
  (subtest "can make"
    (ok worker)
    (is (worker-status worker) :shutdown)
    (is (worker-queue-count worker) 0))

  (subtest "can start"
    (ok (start-worker worker) "start-worker")
    (is (worker-status worker) :idle "status is idle")
    (is (worker-queue-count worker) 0 "queue is empty"))

  (subtest "can stop"
    (ok (stop-worker worker) "stop-worker")
    (is (worker-status worker) :shutdown "status is shutdown")
    (is (worker-queue-count worker) 0 "queue is empty")))

(let* ((bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                        (*error-output* . ,*error-output*)))
       (results (make-array 0 :adjustable t :fill-pointer 0))
       (worker (make-worker (lambda (worker)
                              (sleep 0.1)
                              (multiple-value-bind (val existsp)
                                  (dequeue worker)
                                (when existsp
                                  (vector-push-extend (* val 2) results)))))))
  (subtest "can make"
    (ok worker)
    (is (worker-status worker) :shutdown)
    (is (worker-queue-count worker) 0))

  (subtest "can enqueue"
    (ok (enqueue worker 128) "enqueue")
    (is (worker-status worker) :shutdown "status is still shutdown")
    (is (worker-queue-count worker) 1 "queue count is 1")
    (is results #() :test #'equalp))

  (subtest "can start"
    (ok (start-worker worker) "start-worker")
    (is (worker-status worker) :idle "status is idle")
    (is results #() :test #'equalp))

  (sleep 0.3)

  (subtest "can process"
    (is (worker-status worker) :idle "status is idle")
    (is (worker-queue-count worker) 0 "queue is empty")
    (is results #(256) :test #'equalp)
    (dotimes (i 5)
      (enqueue worker (* i 3)))
    (is (worker-status worker) :running))

  (sleep 1)

  (subtest "can stop"
    (is (worker-queue-count worker) 0 "queue is empty")
    (ok (stop-worker worker) "stop-worker")
    (is (worker-status worker) :shutdown "status is shutdown")
    (is (worker-queue-count worker) 0 "queue is empty")
    (is results #(256 0 6 12 18 24) :test #'equalp)))

(finalize)
