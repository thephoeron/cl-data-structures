(in-package #:cl-user)
(defpackage extrema-tests
  (:use #:cl #:metabang-bind #:prove #:serapeum #:cl-ds
        #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:extrema-tests)

(plan 2)

(bind ((data (cl-ds:xpr (:i 0)
               (when (< i 250)
                 (cl-ds:send-recur i :i (1+ i)))))
       ((min . max) (cl-ds.alg:extrema data #'<)))
  (is min 0)
  (is max 249))

(finalize)
