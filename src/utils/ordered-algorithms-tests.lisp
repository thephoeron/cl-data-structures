(in-package :cl-user)
(defpackage :ordered-algorithms-test-suite (:use :cl :prove))
(in-package :ordered-algorithms-test-suite)

(plan 4)

(let ((data #(1 4 8 16)))
  (is (cl-ds.utils:lower-bound data 3 #'<) 1)
  (is (cl-ds.utils:lower-bound data 20 #'<) 4)
  (is (cl-ds.utils:lower-bound data 0 #'<) 0))

(let ((data #()))
  (is (cl-ds.utils:lower-bound data 3 #'<) 0))

(finalize)
