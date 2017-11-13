(in-package :cl-user)
(defpackage :ordered-algorithms-test-suite (:use :prove))
(in-package :ordered-algorithms-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-ds.utils :ordered-algorithms-test-suite)

(prove:plan 4)

(let ((data #(1 4 8 16)))
  (is (lower-bound data 3 #'<) 1)
  (is (lower-bound data 20 #'<) 4)
  (is (lower-bound data 0 #'<) 0))

(let ((data #()))
  (is (lower-bound data 3 #'<) 0))

(prove:finalize)
