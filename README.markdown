# Legion

[![Build Status](https://img.shields.io/circleci/project/fukamachi/legion.svg)](https://circleci.com/gh/fukamachi/legion)
[![Coverage Status](https://coveralls.io/repos/fukamachi/legion/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/legion)

> Jesus asked him, "What is your name?" And he said, "My name is Legion, for we are many."
> &#x2500;&#x2500; Gospel of Mark chapter 5, verse 9

## Usage

### Worker

```common-lisp
(defparameter *worker*
  (make-worker
    (let ((out *standard-output*))
      (lambda (worker)
        (multiple-value-bind (val existsp)
            (next-job worker)
          (format out "Processed: ~S~%" val))))))

(start-worker *worker*)

(add-job *worker* 10)
(add-job *worker* "Hi")

(stop-worker *worker*)
```

### Cluster

```common-lisp
(defparameter *cluster*
  (make-cluster 4
    (let ((out *standard-output*))
      (lambda (worker)
        (multiple-value-bind (val existsp)
            (next-job worker)
          (format out "Processed: ~S~%" val))))))

(start-cluster *cluster*)

(add-job *cluster* 10)
(add-job *cluster* "Hi")

(stop-cluster *cluster*)
```

NOTE: Cluster doesn't guarantee the order of processing jobs.

## Functions

### \[Structure\] worker

Base structure class of workers.

### \[Function\] (make-worker process-fn &key (queue-size 128))

Create and return a worker thread which has a fixed-length queue. `process-fn` is a funcallable object which takes a single value.

You can specify the value by specifying `:queue-size`. The default value is `128`.

### \[Function\] (worker-status worker)

Return the worker's status which is specifically one of `:running`, `:idle`, `:shutting` and `:shutdown`.

### \[Function\] (worker-queue-count worker)

Return the number of outstanding jobs of the `worker`.

### \[Function\] (start-worker worker)

Start the given `worker` to process jobs.

### \[Function\] (stop-worker worker)

Stop the given `worker` after processing its queued jobs.

### \[Function\] (kill-worker worker)

Stop the given `worker` immediately (outstanding jobs will be remained in its queue).

### \[Function\] (add-job worker-or-cluster val)

Enqueue a new job `val` which will be passed to a function specified for `make-worker`.

### \[Function\] (next-job worker) => val, existsp

Dequeue a job from `worker`'s queue.

### \[Structure\] cluster

Base structure class of clusters.

### \[Function\] (make-cluster worker-num process-fn &key (queue-size 128) scheduler)

Create and return a cluster with `worker-num` workers with `process-fn`.

You can specify a `scheduler` function which takes exact 2 arguments -- workers and a job -- for task-scheduling. The default is round-robin scheduler.

### \[Function\] (cluster-status cluster)

Return the cluster's status which is one of `:running`, `:shutting` and `:shutdown`.

### \[Function\] (start-cluster cluster)

Start workers of `cluster`.

### \[Function\] (cluster-workers cluster) => workers-array

Return workers of `cluster` in simple-array.

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
