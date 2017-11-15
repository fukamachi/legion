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
      (lambda (job)
        (format out "Processed: ~S~%" job)))))

(start *worker*)

(add-job *worker* 10)
(add-job *worker* "Hi")

(stop *worker*)
```

### Cluster

```common-lisp
(defparameter *cluster*
  (make-cluster 4
    (let ((out *standard-output*))
      (lambda (job)
        (format out "Processed: ~S~%" job)))))

(start *cluster*)

(add-job *cluster* 10)
(add-job *cluster* "Hi")

(stop *cluster*)
```

NOTE: Cluster doesn't guarantee the order of processing jobs.

## Functions

### \[Class\] worker

Base class of workers.

You can specify the value by specifying `:queue`.

### \[Function\] (worker-status worker)

Return the worker's status which is specifically one of `:running`, `:idle`, `:shutting` and `:shutdown`.

### \[Function\] (worker-queue-count worker)

Return the number of outstanding jobs of the `worker`.

### \[Method\] (start worker-or-cluster)

Start the given `worker` or `cluster` to process jobs.

### \[Method\] (stop worker-or-cluster)

Stop the given `worker` or `cluster` after processing its queued jobs.

### \[Method\] (kill worker-or-cluster)

Stop the given `worker` or `cluster` immediately (outstanding jobs will be remained in its queue).

### \[Method\] (add-job worker-or-cluster val)

Enqueue a new job `val` which will be passed to a function specified for `make-worker`.

### \[Method\] (fetch-job worker) => val, existsp

Dequeue a job from `worker`'s queue.

### \[Class\] cluster

Base class of clusters.

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

Licensed under the BSD 3-Clause License.
