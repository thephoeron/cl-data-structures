(in-package :cl-user)
(defpackage 2-3-tree-tests (:use :cl :prove :cl-data-structures.aux-package))
(in-package :2-3-tree-tests)

(plan 32)

(let ((node cl-ds.meta:null-bucket)
      (i 31))
  (iterate
    (for i from 0 below 32)
    (for new = (lambda () i))
    (setf node (cl-data-structures.common.2-3-tree::insert-front-into-tree
                node
                new)))
  (labels ((check (node)
             (etypecase node
               (cl-data-structures.common.2-3-tree::3-node
                (check (cl-data-structures.common.2-3-tree::access-left node))
                (check (cl-data-structures.common.2-3-tree::access-middle node))
                (check (cl-data-structures.common.2-3-tree::access-right node)))
               (cl-data-structures.common.2-3-tree::2-node
                (check (cl-data-structures.common.2-3-tree::access-left node))
                (check (cl-data-structures.common.2-3-tree::access-right node)))
               (t
                (is node i)
                (decf i)))))
    (check node)
    ))

(finalize)
