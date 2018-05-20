(in-package #:cl-user)
(defpackage mutual-information-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:mutual-information-tests)

(plan 3)

(let* ((data #((1 . 2) (3 . 2) (4 . 2) (5 . 2)))
       (mi (cl-ds.math:mutual-information data
                                          (cl-ds:field :name :first
                                                       :type :discrete
                                                       :key #'car)
                                          (cl-ds:field :name :second
                                                       :type :discrete
                                                       :key #'cdr))))
  (is (cl-ds:at mi :second) 0.0 :test #'=))

(let* ((data #((1 . 2) (3 . 2) (4 . 2) (5 . 2)))
       (mi (cl-ds.math:mutual-information data
                                          (cl-ds:field :name :first
                                                       :type :continues
                                                       :split-points-count 8
                                                       :key #'car)
                                          (cl-ds:field :name :second
                                                       :type :discrete
                                                       :key #'cdr))))
  (is (cl-ds:at mi :second) 0.0 :test #'=))

(let* ((data #((1 . 2) (3 . 2) (4 . 1) (2 . 3) (2 . 3) (2 . 3)))
       (mi (cl-ds.math:optimal-split-point data
                                           (cl-ds:field :name :first
                                                        :type :continues
                                                        :split-points-count 3
                                                        :key #'car)
                                           (cl-ds:field :name :second
                                                        :type :discrete
                                                        :key #'cdr))))
  (is (car (cl-ds:at mi :second)) 3 :test #'=))

(finalize)
