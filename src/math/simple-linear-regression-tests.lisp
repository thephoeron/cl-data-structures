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
       ((beta1 beta0) (cl-ds.math::simple-linear-regression
                       xpr
                       #'first
                       #'second)))
  (is beta1 2 :test #'=)
  (is beta0 1 :test #'=))


(finalize)
