(cl:in-package #:cl-user)
(defpackage extrema-tests
  (:use #:cl #:prove #:cl-ds #:cl-data-structures.aux-package))

(in-package #:extrema-tests)

(plan 2)

(bind ((data (cl-ds:xpr (:i 0)
               (when (< i 250)
                 (cl-ds:send-recur i :i (1+ i)))))
       ((min . max) (cl-ds.alg:extrema data #'<)))
  (is min 0)
  (is max 249))

(finalize)
