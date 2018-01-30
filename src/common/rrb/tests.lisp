(in-package :cl-user)
(defpackage rrb-test-suite
  (:use :prove :cl))
(in-package :rrb-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-data-structures.common.rrb :rrb-test-suite)

(progn
  (prove:plan 2)
  (is (tail-offset +maximum-children-count+) 0)
  (is (tail-offset 500) 480)
  (prove:finalize))
