(in-package #:cl-user)
(defpackage simple-linear-regression-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:simple-linear-regression-tests)


(plan 2)


(bind ((xpr (xpr (:x 0)
              (when (< x 100)
                (send-recur (list x (1+ (* 2 x)))
                            :x (1+ x)))))
       (result (cl-ds.math::simple-linear-regression
                xpr
                #'first
                #'second)))
  (is (cl-ds.math:beta1 result) 2 :test #'=)
  (is (cl-ds.math:beta0 result) 1 :test #'=))


(finalize)
