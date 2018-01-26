(in-package :cl-user)
(defpackage rrb-vector-tests (:use :prove))
(in-package :rrb-vector-tests)
(cl-ds.utils:import-all-package-symbols :cl-data-structures.sequences.rrb-vector :rrb-vector-tests)


(plan 34)
(let* ((container (make-instance 'functional-rrb-vector))
       (cont1 (cl-ds:put container 1))
       (cont2 (cl-ds:put cont1 2)))
  (is (cl-ds:at cont2 0) 1)
  (is (cl-ds:at cont2 1) 2)
  (setf container cont2)
  (iterate
    (for i from 3)
    (repeat cl-data-structures.common.rrb:+maximum-children-count+)
    (setf container (cl-ds:put container i)))
  (iterate
    (for i from 3)
    (repeat cl-data-structures.common.rrb:+maximum-children-count+)
    (is (cl-ds:at container (1- i)) i)))
(finalize)
