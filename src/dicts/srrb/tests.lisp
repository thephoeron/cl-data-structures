(in-package :cl-user)
(defpackage sparse-rrb-vector-tests
  (:use :cl :prove :cl-data-structures.aux-package)
  (:shadowing-import-from :iterate :collecting :summing :in))
(in-package :sparse-rrb-vector-tests)

(plan 12)

(let* ((tail (make-array cl-ds.common.rrb:+maximum-children-count+))
       (vector (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector
                              :tail tail
                              :tail-mask #b111
                              :index-bound 32)))
  (setf (aref tail 0) 1
        (aref tail 1) 2
        (aref tail 2) 3
        (aref tail 3) 4)
  (is (cl-ds.dicts.srrb::access-tree vector) nil)
  (cl-ds.dicts.srrb::insert-tail! vector nil)
  (ok (cl-ds.dicts.srrb::access-tree vector))
  (is (cl-ds:size vector) 3)
  (is (cl-ds.dicts.srrb::access-tree-index-bound vector) 32)
  (is (cl-ds.dicts.srrb::access-tree-size vector) 3)
  (let* ((tree (cl-ds.dicts.srrb::access-tree vector))
         (content (cl-ds.common.rrb:sparse-rrb-node-content tree)))
    (is (cl-ds.common.rrb:sparse-rrb-node-bitmask tree)
        #b111)
    (is (length content)
        3)
    (is (aref content 0) 1)
    (is (aref content 1) 2)
    (is (aref content 2) 3))
  (setf (cl-ds.dicts.srrb::access-tail vector) (make-array cl-ds.common.rrb:+maximum-children-count+)
        (cl-ds.dicts.srrb::access-tail-mask vector) #b1)
  (cl-ds.dicts.srrb::insert-tail! vector nil)
  (let* ((tree (cl-ds.dicts.srrb::access-tree vector))
         (content (cl-ds.common.rrb:sparse-rrb-node-content tree)))
    (is (cl-ds.common.rrb:sparse-rrb-node-bitmask tree) #b11)
    (is (length content) 2)))

(finalize)
