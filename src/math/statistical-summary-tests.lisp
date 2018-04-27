(in-package #:cl-user)
(defpackage statistical-summary-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:statistical-summary-tests)


(plan 5)


(bind ((data #(1 3 5))
       (summary (cl-ds.math:statistical-summary data)))
  (is (cl-ds:size summary) 4)
  (is (cl-ds:at summary :average) 3)
  (is (cl-ds:at summary :variance) 8/3)
  (is (cl-ds:at summary :skewness) 0 :test #'=)
  (is (cl-ds:at summary :kurtosis) 1.5 :test
      (lambda (a b)
        (< (abs (- a b)) 0.000001))))


(finalize)
