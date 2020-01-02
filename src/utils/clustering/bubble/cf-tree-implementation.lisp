(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun distance (tree first-item second-item)
  (funcall (read-distance-function tree) first-item second-item))


(defun average-inter-cluster-distance* (distance-function
                                        first-leaf
                                        second-leaf)
  (declare (optimize (speed 3)))
  (ensure-functionf distance-function)
  (iterate outer
    (declare (type (cl-ds.utils:extendable-vector t)
                   first-content second-content)
             (type fixnum i first-length second-length))
    (with first-content = (read-content first-leaf))
    (with second-content = (read-content second-leaf))
    (with first-length = (length first-content))
    (with second-length = (length second-content))
    (for i from 0 below first-length)
    (for first-elt = (aref first-content i))
    (iterate
      (declare (type fixnum j))
      (for j from 0 below second-length)
      (for second-elt = (aref second-content j))
      (in outer (sum (funcall distance-function first-elt second-elt) into result)))
    (finally (return-from outer
               (~> result
                   (/ (* first-length second-length))
                   (/ 2))))))


(defmethod clusteroid ((tree cf-tree) (node cf-leaf))
  (aref (read-content node) 0))


(defmethod clusteroid ((tree cf-tree) (node cf-subtree))
  (aref (access-sample node) 0))


(defmethod clusteroid-distance ((tree cf-tree)
                                (first-node fundamental-cf-node)
                                (second-node fundamental-cf-node))
  (distance tree
            (clusteroid tree first-node)
            (clusteroid tree second-node)))


(defmethod clusteroid-distance ((tree cf-tree)
                                (first-node fundamental-cf-node)
                                (item t))
  (distance tree
            (clusteroid tree first-node)
            item))


(defmethod position-of-clusteroid ((tree cf-tree) (vector vector))
  (iterate
    (with length = (length vector))
    (for i from 0 below length)
    (for first = (aref vector i))
    (for distance-sum
         = (iterate
             (for j from 0 below length)
             (when (= i j) (next-iteration))
             (sum (distance tree first (aref vector j)))))
    (finding i minimizing distance-sum)))


(defmethod needs-resampling-p ((tree cf-tree) (leaf cf-leaf))
  nil)


(defmethod needs-resampling-p ((tree cf-tree) (subtree cf-subtree))
  (>= (access-inserts subtree) (read-sampling-rate tree)))


(defmethod needs-split-p ((tree cf-tree) (subtree cf-subtree))
  (>= (~> subtree read-children length)
      (read-subtree-maximum-arity tree)))


(defmethod needs-split-p ((tree cf-tree) (leaf cf-leaf))
  (or (>= (~> leaf read-content length)
          (read-leaf-maximum-size tree))
      (>= (access-radius leaf)
          (read-leaf-maximum-radius tree))))


(defmethod average-inter-cluster-distance ((tree cf-tree)
                                           (first-leaf cf-leaf)
                                           (second-leaf cf-leaf))
  (average-inter-cluster-distance* (read-distance-function tree)
                                   first-leaf second-leaf))
