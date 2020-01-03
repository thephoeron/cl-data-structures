(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun parallel-bubble-grouping (data tree)
  cl-ds.utils:todo)


(defun single-thread-bubble-grouping (data tree)
  cl-ds.utils:todo)


(defun gather-leafs (tree root &key (key #'identity))
  (lret ((result (vect)))
    (visit-leafs tree root
                 (rcurry #'vector-push-extend result)
                 :key key)))


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
                      :key (lambda (node) (leaf-content tree node))))))
