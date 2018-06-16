(in-package #:cl-user)
(defpackage apriori-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:apriori-tests)


(let* ((data #((1 2) (1 4) (1 2 4) (3 4)
               (1 3) (1 3) (1 3 4) (1 3 2)))
       (index (cl-ds.counting:apriori data
                                      1
                                      0.1
                                      :parallel nil)))
  (defparameter *index* index))
