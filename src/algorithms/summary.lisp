(in-package #:cl-data-structures.algorithms)


(defun make-summary-aggregation-function (lambdas)
  (iterate
    (for (symbol function callable) in lambdas)
    (unless (typep function 'cl-ds.alg.meta:aggregation-function)
      (error 'cl-ds:invalid-argument
             :argument symbol
             :text (format nil "~a is not an aggregation function!" symbol)))))


(defun %summary (range lambdas)
  (cl-ds.alg.meta:apply-range-function
   range (make-summary-aggregation-function lambdas)))


(defmacro summary (range &body functions))
