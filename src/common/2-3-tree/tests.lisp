(in-package :cl-user)
(defpackage 2-3-tree-tests (:use :cl :prove :cl-data-structures.aux-package))
(in-package :2-3-tree-tests)

(plan 32)

(let ((node nil)
      (i 31))
  (iterate
    (for i from 0 below 32)
    (for new = (lambda () i))
    (setf node (cl-data-structures.common.2-3-tree::insert-front-handle-nil
                (cl-data-structures.common.2-3-tree::insert-front
                 new
                 node)
                node
                new)))
  (labels ((check (node)
             (etypecase node
               (cl-data-structures.common.2-3-tree::3-node
                (progn
                  (check (cl-data-structures.common.2-3-tree::access-left node))
                  (is (cl-data-structures.common.2-3-tree::access-content-1 node) i)
                  (decf i)
                  (check (cl-data-structures.common.2-3-tree::access-center node))
                  (is (cl-data-structures.common.2-3-tree::access-content-2 node) i)
                  (decf i)
                  (check (cl-data-structures.common.2-3-tree::access-right node))))
               (cl-data-structures.common.2-3-tree::2-node
                (progn
                  (check (cl-data-structures.common.2-3-tree::access-left node))
                  (is (cl-data-structures.common.2-3-tree::access-content-1 node) i)
                  (decf i)
                  (check (cl-data-structures.common.2-3-tree::access-right node))))
               (cl-data-structures.common.2-3-tree::2-content
                (progn (is (cl-data-structures.common.2-3-tree::access-content-1 node) i)
                       (decf i)
                       (is (cl-data-structures.common.2-3-tree::access-content-2 node) i)
                       (decf i)))
               (cl-data-structures.common.2-3-tree::1-content
                (progn (is (cl-data-structures.common.2-3-tree::access-content-1 node) i)
                       (decf i))))))
    (check node)))

(finalize)
