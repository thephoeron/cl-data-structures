(in-package #:cl-user)
(defpackage summary-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:summary-tests)

(plan 2)

(let* ((data (cl-ds:xpr (:i 0)
               (when (< i 250)
                 (cl-ds:send-recur i :i (1+ i)))))
       (min-and-max (cl-ds.alg:summary data
                      :min (cl-ds.alg:accumulate #'min)
                      :max (cl-ds.alg:accumulate #'max))))
  (is (cl-ds:at min-and-max :min) 0)
  (is (cl-ds:at min-and-max :max) 249))

(finalize)
