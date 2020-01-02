(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun distance (tree first-item second-item)
  (funcall (read-distance-function tree) first-item second-item))


(defun cf-leaf-update-row-sums (tree leaf)
  (declare (optimize (speed 3)))
  (let* ((distance-function (read-distance-function tree))
         (content (read-content leaf))
         (row-sums (read-row-sums tree))
         (length (length content))
         (new-row-sum 0)
         (last-index (1- length))
         (new-element (aref content last-index)))
    (declare (type (cl-ds.utils:extendable-vector t) content row-sums)
             (type fixnum length last-index)
             (type function distance-function))
    (iterate
      (declare (type fixnum i))
      (for i from 0 below (1- length))
      (for distance = (funcall distance-function
                               (aref content i)
                               new-element))
      (incf new-row-sum distance)
      (incf (aref row-sums i) distance))
    (vector-push-extend row-sums row-sums)
    nil))


(defun cf-leaf-update-clusteroid (tree leaf)
  (declare (optimize (speed 3)))
  (let* ((content (read-content leaf))
         (row-sums (read-row-sums tree))
         (length (length content))
         (new-clusteroid (iterate
                           (declare (type fixnum i))
                           (for i from 0 below length)
                           (for row-sum = (aref row-sums i))
                           (finding i minimizing row-sum))))
    (declare (type cl-ds.utils:extendable-vector content row-sums)
             (type fixnum length))
    (rotatef (aref content new-clusteroid)
             (aref content 0))
    (rotatef (aref row-sums new-clusteroid)
             (aref row-sums 0))
    nil))


(defun cf-subtree-update-clusteroid (tree subtree)
  (iterate
    (with sample = (access-sample subtree))
    (with length = (length sample))
    (with distance-function = (read-distance-function tree))
    (for i from 0 below length)
    (for first-elt = (aref sample i))
    (for row-sum = (+ (iterate
                        (for j from 0 below i)
                        (sum (funcall distance-function
                                      first-elt
                                      (aref sample j))))
                      (iterate
                        (for j from i below length)
                        (sum (funcall distance-function
                                      first-elt
                                      (aref sample j))))))
    (finding i minimizing row-sum into position)
    (finally (rotatef (aref sample position) (aref sample 0)))))


(defun cf-leaf-calculate-radius (tree leaf)
  (let* ((distance-function (read-distance-function tree))
         (content (read-content leaf))
         (clusteroid (aref content 0)))
    (declare (type (cl-ds.utils:extendable-vector t) content)
             (type function distance-function))
    (~> (reduce #'+ content :start 1 :key
                (lambda (x) (funcall distance-function clusteroid x)))
        (/ (length content))
        sqrt)))

(defun average-inter-cluster-distance* (distance-function
                                        first-leaf
                                        second-leaf)
  (declare (optimize (speed 3) (safety 0)))
  (ensure-functionf distance-function)
  (iterate
    (declare (type (cl-ds.utils:extendable-vector t)
                   first-content second-content)
             (type number result)
             (type fixnum i first-length second-length))
    (with first-content = (read-content first-leaf))
    (with second-content = (read-content second-leaf))
    (with first-length = (length first-content))
    (with second-length = (length second-content))
    (with result = 0)
    (for i from 0 below first-length)
    (for first-elt = (aref first-content i))
    (iterate
      (declare (type fixnum j))
      (for j from 0 below second-length)
      (for second-elt = (aref second-content j))
      (incf result (funcall distance-function first-elt second-elt)))
    (finally (return
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
