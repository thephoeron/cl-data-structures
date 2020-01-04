(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun insert-into-tree (tree root element)
  (let ((stack (list (cons root nil))))
    (iterate
      (with node = root)
      (for next = (cf-insert tree node element))
      (while (consp next))
      (push next stack)
      (setf node (car node)))
    (iterate
      (for cell on stack)
      (for p-cell previous cell)
      (for p-node = (caar p-cell))
      (for ((node . position) . rest) = cell)
      (unless (or (null p-node)
                  (typep p-node 'fundamental-cf-node))
        (assert (vectorp p-node))
        (absorb-nodes tree node p-node position))
      (for at-root = (endp rest))
      (if (needs-split-p tree node)
          (let ((splitted (split tree node)))
            (if at-root
                (let ((new-root (make-subtree tree)))
                  (absorb-nodes tree new-root splitted)
                  (leave new-root))
                (setf (caar cell) splitted)))
          (leave root)))))


(defun single-thread-bubble-grouping (data tree)
  (let ((root (make-leaf data)))
    (iterate
      (for d in-vector data)
      (setf root (insert-into-tree tree root d))
      (finally (return root)))))


(defun gather-leafs (tree root &key (key #'identity))
  (lret ((result (vect)))
    (visit-leafs tree root
                 (rcurry #'vector-push-extend result)
                 :key key)))


(defun parallel-bubble-grouping (data tree)
  cl-ds.utils:todo)


(defun bubble-grouping (data
                        distance-function
                        sampling-rate
                        sample-size
                        subtree-maximum-arity
                        leaf-maximum-size
                        leaf-maximum-radius
                        &key (parallel nil))
  (let ((tree (make 'cf-tree :distance-function distance-function
                             :sampling-rate sampling-rate
                             :subtree-maximum-arity subtree-maximum-arity
                             :leaf-maximum-size leaf-maximum-size
                             :leaf-maximum-radius leaf-maximum-radius
                             :sample-size sample-size)))
    (~> (if parallel
            (parallel-bubble-grouping data tree)
            (single-thread-bubble-grouping data tree))
        (gather-leafs tree _
                      :key (lambda (x)
                             (make 'bubble
                                   :content (leaf-content tree x)))))))
