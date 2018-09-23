(in-package :cl-user)
(defpackage 2-3-queue-tests (:use :cl :prove :cl-data-structures.aux-package))
(in-package :2-3-queue-tests)

(plan 100)

(let ((queue (make 'cl-ds.queues.2-3-tree::mutable-2-3-queue)))
  (iterate
    (for i from 0 below 100)
    (cl-ds:put! queue i))
  (iterate
    (for i from 0 below 100)
    (cl-ds:mod-bind (container found value) (cl-ds:take-out! queue)
      (is value i))))

(finalize)
