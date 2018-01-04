(in-package :cl-user)
(defpackage :distances-test-suite (:use :prove))
(in-package :distances-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-ds.utils :distances-test-suite)

(defun jaccard-metric (a b)
  (if (eql a b)
      1.0
      (coerce (/ (logcount (logand a b))
                 (logcount (logior a b)))
              'single-float)))

(prove:plan 26)

(iterate
  (for i from 0 below 5)
  (with prev = -1)
  (iterate (for j from (1+ i) below 5)
    (for next = (index-in-content-of-distance-matrix 5 i j))
    (is next (1+ prev))
    (setf prev next)))


(cl-ds.utils:with-vectors ((data #(0 1 2 3 4 5)))
  (let ((matrix (cl-ds.utils:make-distance-matrix-from-vector 'single-float #'jaccard-metric data)))
    (iterate
      (for i below 5)
      (is (cl-ds.utils:distance matrix i i) 0.0 :test #'=))
    (iterate
      (for i below 5)
      (iterate
        (for j from (1+ i) below 5)
        (is (cl-ds.utils:distance matrix i j)
            (jaccard-metric (data i)
                            (data j))
            :test #'=)))
    (setf (cl-ds.utils:distance matrix 1 2) 5.0)
    (is (cl-ds.utils:distance matrix 1 2) 5.0)))

(prove:finalize)
