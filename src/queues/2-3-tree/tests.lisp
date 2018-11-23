(in-package :cl-user)
(defpackage 2-3-queue-tests (:use :cl :prove :cl-data-structures.aux-package))
(in-package :2-3-queue-tests)

(plan 1188)

(let ((queue (make 'cl-ds.queues.2-3-tree::mutable-2-3-queue)))
  (iterate
    (for i from 0 below 100)
    (is (cl-ds:size queue) i)
    (cl-ds:put! queue i)
    (is (cl-ds:at queue :back) i))
  (iterate
    (for i from 0 below 100)
    (is (cl-ds:size queue) (- 100 i))
    (cl-ds:mod-bind (container found value) (cl-ds:take-out! queue)
      (is value i))))

(let* ((queue (make 'cl-ds.queues.2-3-tree:functional-2-3-queue))
       (og-queue nil)
       (empty queue))
  (iterate
    (for i from 0 below 100)
    (is (cl-ds:size queue) i)
    (setf queue (cl-ds:put queue i))
    (is (cl-ds:at queue :back) i))
  (let ((i 0))
    (cl-ds:across (lambda (s) (is s i) (incf i))
                  queue)
    (is i 100))
  (is (cl-ds:size empty) 0)
  (setf og-queue queue)
  (iterate
    (for i from 0 below 100)
    (is (cl-ds:size queue) (- 100 i))
    (cl-ds:mod-bind (container found value) (cl-ds:take-out queue)
      (setf queue container)
      (is value i)))
  (setf queue og-queue)
  (iterate
    (for i from 0 below 100)
    (is (cl-ds:size queue) (- 100 i))
    (cl-ds:mod-bind (container found value) (cl-ds:take-out queue)
      (setf queue container)
      (is value i))))

(let ((queue (make 'cl-ds.queues.2-3-tree::mutable-2-3-queue)))
  (iterate
    (for i from 0 below 20)
    (is (cl-ds:size queue) i)
    (cl-ds:put! queue i)
    (is (cl-ds:at queue :back) i))
  (iterate
    (for i from 0 below 5)
    (cl-ds:mod-bind (container found value) (cl-ds:take-out! queue)
      (is value i)))
  (iterate
    (for i from 0 below 15)
    (cl-ds:put! queue i)
    (is (cl-ds:at queue :back) i))
  (iterate
    (for i from 5 below 20)
    (cl-ds:mod-bind (container found value) (cl-ds:take-out! queue)
      (is value i)))
  (iterate
    (for i from 0 below 15)
    (cl-ds:mod-bind (container found value) (cl-ds:take-out! queue)
      (is value i))))

(finalize)
