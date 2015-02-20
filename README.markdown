# Simple Worker

Simple multithreading worker mechanism.

## Usage

```common-lisp
(defparameter *worker*
  (make-worker
   (let ((out *standard-output*))
     (lambda (worker)
       (multiple-value-bind (val existsp)
           (dequeue worker)
         (format out "Processed: ~S~%" val))))))

(start-worker *worker*)

(enqueue *worker* 10)
(enqueue *worker* "Hi")

(stop-worker *worker*)
```

## Functions

### \[Structure\] worker

Base structure class.

### \[Function\] (make-worker process-fn &key (queue-size 128))

Create and return a worker thread which has a fixed-length queue. `process-fn` is a funcallable object which takes single value.

You can specify the value by specifying `:queue-size`. The default value is `128`.

### \[Function\] (worker-status worker)

Return the worker's status which is one of `:running`, `:idle`, `:shutting` and `:shutdown`.

### \[Function\] (worker-queue-count worker)

Return the number of outstanding jobs.

### \[Function\] (start-worker worker)

Start the given `worker`.

### \[Function\] (stop-worker worker)

Stop the given `worker` after processing its queued jobs.

### \[Function\] (kill-worker worker)

Stop the given `worker` immediately.

### \[Function\] (enqueue worker val)

Enqueue a new job `val`. It will be passed to a function specified for `make-worker`.

### \[Function\] (dequeue worker) => val, existsp

Dequeue a job from `worker`'s queue.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
