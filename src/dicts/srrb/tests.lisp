(in-package :cl-user)
(defpackage sparse-rrb-vector-tests
  (:use :cl :prove :cl-data-structures.aux-package)
  (:shadowing-import-from :iterate :collecting :summing :in))
(in-package :sparse-rrb-vector-tests)

(plan 1)

(let* ((tail (make-array (cl-ds.common.rrb:+maximum-children-count+)))
       (vector (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector
                              :tail tail)))
  (cl-ds.dicts.srrb::insert-tail! vector nil)
  (is (cl-ds.dicts.srrb::access-tree vector) tail))

(finalize)
