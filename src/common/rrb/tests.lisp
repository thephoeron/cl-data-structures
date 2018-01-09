(in-package :cl-user)
(defpackage rrb-test-suite
  (:use :prove))
(in-package :rrb-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-data-structures.common.rrb :rrb-test-suite)

(prove:plan 2)
(is (tail-offset 32) 0)
(is (tail-offset 500) 480)
(prove:finalize)
