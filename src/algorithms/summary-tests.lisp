(in-package #:cl-user)
(defpackage summary-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:summary-tests)

(plan 2)

(let* ((data (cl-ds:xpr (:i 0)
               (when (< i 250)
                 (cl-ds:send-recur i :i (1+ i)))))
       (min-and-max (cl-ds.alg:summary
                     data
                     (list (cl-ds:field :name :min
                                        :fn (list #'cl-ds.alg:accumulate
                                                  #'min
                                                  :range))
                           (cl-ds:field :name :max
                                        :fn (list #'cl-ds.alg:accumulate
                                                  #'max
                                                  :range))))))
  (is (cl-ds:at min-and-max :min) 0)
  (is (cl-ds:at min-and-max :max) 249))

(finalize)
