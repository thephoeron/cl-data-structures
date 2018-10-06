(in-package #:cl-user)
(defpackage statistical-summary-tests
  (:use :cl :prove :cl-data-structures.aux-package))

(in-package #:statistical-summary-tests)


(plan 5)

(defun delta-comparsion (a b)
  (< (abs (- a b)) 0.000001))


(bind ((data #(1 3 5 3 2 3))
       (summary (cl-ds.math:statistical-summary data)))
  (is (cl-ds:size summary) 4)
  (is (cl-ds:at summary :average) (alexandria:mean data) :test #1=#'delta-comparsion)
  (is (cl-ds:at summary :variance) (alexandria:variance data) :test #1#)
  (is (cl-ds:at summary :skewness) 0.321372 :test #1#)
  (is (cl-ds:at summary :kurtosis) 2.600569 :test #1#))

(finalize)
