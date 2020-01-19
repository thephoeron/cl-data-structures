(cl:in-package #:cl-user)
(defpackage summary-tests
  (:use #:cl #:prove #:cl-ds #:cl-data-frames.aux-package))

(cl:in-package #:summary-tests)

(plan 4)

(let* ((data (cl-ds:xpr (:i 0)
               (when (< i 250)
                 (cl-ds:send-recur i :i (1+ i)))))
       (min-and-max (cl-ds.alg:summary (cl-ds.alg:group-by data :key #'evenp)
                      :min (cl-ds.alg:accumulate #'min)
                      :max (cl-ds.alg:accumulate #'max))))
  (is (cl-ds:at min-and-max :min) 0)
  (is (cl-ds:at min-and-max :max) 249))

(let* ((data (~> (cl-ds:iota-range :to 50)
                 (cl-ds.alg:group-by :key #'oddp)))
       (summary (cl-ds.alg:summary data
                  :vector (cl-ds.alg:to-vector))))
  (is (~> summary (cl-ds:at nil) (cl-ds:at :vector) length)
      25)
  (is (~> summary (cl-ds:at t) (cl-ds:at :vector) length)
      25))

(finalize)
