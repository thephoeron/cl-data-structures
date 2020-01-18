(cl:in-package #:cl-user)
(defpackage on-each-tests
  (:use :cl :prove :cl-data-structures.aux-package))

(in-package #:on-each-tests)

(plan 2)

(let* ((data #(1 2 3 4))
       (range (cl-ds.alg:on-each (cl-ds:whole-range data) #'1+)))
  (is (cl-ds.alg:accumulate range #'+) (+ 2 3 4 5)))

(is (cl-ds.alg:accumulate
     (cl-ds.alg:on-each (cl-ds:xpr (:i 1)
                          (when (< i 5)
                            (cl-ds:send-recur i :i (1+ i))))
                        #'1+)
     #'+)
    (+ 2 3 4 5))

(finalize)
