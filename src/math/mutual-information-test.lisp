(in-package #:cl-user)
(defpackage mutual-information-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:mutual-information-tests)

(plan 1)

(let* ((data #((1 . 2) (3 . 2) (4 . 2) (5 . 2)))
       (mi (cl-ds.math:mutual-information data
                                          (cl-ds:field :name :first :type :discrete :key #'car)
                                          (cl-ds:field :name :second :type :discrete :key #'cdr))))

  (is (cl-ds:at mi :second) 0.0 :test #'=))

(finalize)
