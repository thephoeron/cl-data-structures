(cl:in-package :cl-user)
(defpackage :lazy-shuffe-test-suite (:use :prove :cl :iterate))
(cl:in-package :lazy-shuffe-test-suite)

(prove:plan 1)

(let* ((generator (cl-ds.utils:lazy-shuffle 0 5))
       (data (iterate
               (for elt = (funcall generator))
               (while elt)
               (collect elt))))
  (is '(0 1 2 3 4) (sort data #'<) :test #'equal))

(prove:finalize)
