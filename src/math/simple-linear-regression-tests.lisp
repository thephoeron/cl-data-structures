(in-package #:cl-user)
(defpackage simple-linear-regression-tests
  (:use :cl-ds :cl :prove :cl-data-structures.aux-package))

(in-package #:simple-linear-regression-tests)


(plan 2)


(bind ((xpr (xpr (:x 0)
              (when (< x 100)
                (send-recur (list x (1+ (* 2 x)))
                            :x (1+ x)))))
       (result (cl-ds.math::simple-linear-regression
                xpr
                #'first
                (cl-ds.math:average xpr :key #'first)
                #'second
                (cl-ds.math:average xpr :key #'second))))
  (is (cl-ds.math:beta1 result) 2 :test #'=)
  (is (cl-ds.math:beta0 result) 1 :test #'=))


(finalize)
