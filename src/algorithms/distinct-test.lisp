(cl:in-package #:cl-user)
(defpackage distinct-tests
  (:use :cl :prove :cl-data-structures.aux-package))

(in-package #:distinct-tests)

(plan 3)

(let* ((input '((0 . 1) (0 . 2) (0 . 1) (1 . 1) (1 . 1) (1 . 2)))
       (result (serapeum:~> input
                            (cl-ds.alg:group-by :key #'car)
                            (cl-ds.alg:distinct :key #'cdr)
                            cl-ds.alg:to-list)))
  (is (cl-ds:size result) 2)
  (is (cl-ds:at result 0) '((0 . 1) (0 . 2)) :test #'equal)
  (is (cl-ds:at result 1) '((1 . 1) (1 . 2)) :test #'equal))

(finalize)
