(in-package #:cl-user)

(defpackage chi-squared-test (:use #:prove #:cl))
(in-package #:chi-squared-test)

(plan 1)

(let* ((data (list #2A((10 50) (80 20))))
       (pval (cl-ds.math:chi-squared data #'aref
                                     (list (cl-ds:field :test 'eql
                                                        :classes '(0 1))
                                           (cl-ds:field :test 'eql
                                                        :classes '(0 1))))))
  (ok (< pval 0.05)))

(finalize)
