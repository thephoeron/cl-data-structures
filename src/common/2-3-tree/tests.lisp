(cl:in-package :cl-user)
(defpackage 2-3-tree-tests (:use :cl :prove :cl-data-structures.aux-package))
(cl:in-package :2-3-tree-tests)

(plan 769)

(let ((node cl-ds.meta:null-bucket)
      (i 127))
  (iterate
    (for i from 0 below 128)
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
    (iterate
      (for i from 0 below 128)
      (for (values new-node old-value) = (cl-ds.common.2-3::delete-back-from-tree node))
      (setf node new-node)
      (is i old-value))
    (is node cl-ds.meta:null-bucket)))

(let* ((tree (make 'cl-data-structures.common.2-3-tree::tree))
       (i 127))
  (iterate
    (for i from 0 below 128)
    (for new = (lambda () i))
    (cl-data-structures.common.2-3-tree::insert-front-into-tree!
     tree
     new))
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
    (check (cl-data-structures.common.2-3-tree::access-root tree))
    (iterate
      (for i from 0 below 128)
      (for (values new-node old-value) = (cl-ds.common.2-3::delete-back-from-tree! tree))
      (is i old-value))))

(let* ((tree (make 'cl-data-structures.common.2-3-tree::tagged-tree))
       (i 127))
  (iterate
    (for i from 0 below 128)
    (for new = (lambda () i))
    (cl-data-structures.common.2-3-tree::transactional-insert-front-into-tree!
     tree
     new))
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
    (check (cl-data-structures.common.2-3-tree::access-root tree))
    (iterate
      (for i from 0 below 128)
      (for (values new-node old-value) = (cl-ds.common.2-3::transactional-delete-back-from-tree! tree))
      (is i old-value))))

(finalize)
