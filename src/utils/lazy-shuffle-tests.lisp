(in-package :cl-user)
(defpackage :lazy-shuffe-test-suite (:use :prove))
(in-package :lazy-shuffe-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-ds.utils :lazy-shuffe-test-suite)

(prove:plan 1)

(let* ((generator (lazy-shuffle 0 5))
       (data (iterate
               (for elt = (funcall generator))
               (while elt)
               (collect elt))))
  (is '(0 1 2 3 4) (sort data #'<) :test #'equal))

(prove:finalize)
