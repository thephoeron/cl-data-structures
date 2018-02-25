(in-package #:cl-data-structures.common.egnat)


(defclass fundamental-egnat ()
  ((%branching-factor
    :type non-negative-fixnum
    :initarg :branching-factor
    :reader read-branching-factor)
   (%metric-fn
    :reader read-metric-fn
    :initarg :metric-fn)
   (%metric-type
    :reader read-metric-type
    :initform :single-float
    :initarg :metric-type)
   (%content-count-in-node
    :type non-negative-fixnum
    :reader read-content-count-in-node
    :initarg :content-count-in-node)
   (%root
    :reader read-root
    :initform nil
    :initarg :root)))


(defclass egnat-node ()
  ((%ranges
    :type cl-ds.utils:distance-matrix
    :initarg :ranges
    :reader read-ranges)
   (%content
    :type vector
    :initarg :content
    :reader read-content)
   (%children
    :type (or null (vector memory-gnat-node))
    :initform nil
    :initarg :children
    :reader read-children)))


(defun force-tree (node)
  (bind (((:slots %children) node))
    (unless (null %children)
      (map-into %children #'lparallel:force %children)
      (map nil #'force-tree %children))))


(defun make-future-egnat-nodes (data number-of-nodes number-of-content metric-fn)
  (if (<= (length data) number-of-content)
      (make 'egnat-node :content data)
      (bind ((this-content (take number-of-content data))
             (data (drop (length this-content) data))
             (seeds-indexes (iterate
                              (with generator = (cl-ds.utils:lazy-shuffle 0 (length data)))
                              (with result = (make-array number-of-nodes
                                                         :element-type 'fixnum
                                                         :fill-pointer 0
                                                         :adjustable t))
                              (for i = (funcall generator))
                              (repeat number-of-nodes)
                              (while i)
                              (vector-push-extend (aref data i) result)
                              (finally (return result))))
             (reverse-mapping (let ((table (make-hash-table)))
                                (iterate
                                  (for i in-vector seeds-indexes)
                                  (for j from 0)
                                  (setf (gethash i table) j))
                                table))
             (data-partitions (map '(vector fixnum)
                                   (lambda (x)
                                     (let ((data (aref data x)))
                                       (iterate
                                         (for seed-index in-vector seeds-indexes)
                                         (for seed = (aref data seed-index))
                                         (for distance = (funcall metric-fn data seed))
                                         (minimize distance into min)
                                         (finding seed-index such-that (= distance min)))))
                                   (range 0 (length data))))
             (contents (let ((result (make-array number-of-nodes
                                                 :adjustable t
                                                 :fill-pointer (length seeds-indexes))))
                         (map-into result #'vect)
                         (iterate
                           (for d in-vector data-partitions)
                           (for i from 0)
                           (for element = (aref data i))
                           (for partition = (gethash d reverse-mapping))
                           (vector-push-extend element (aref result partition)))
                         result))
             (children (map-into
                        (make-array number-of-nodes
                                    :adjustable t
                                    :fill-pointer (length seeds-indexes))
                        (lambda (content)
                          (lparallel:future
                            (make-future-egnat-nodes content number-of-nodes
                                                     number-of-content metric-fn)))
                        contents))
             (ranges (cl-ds.utils:make-distance-matrix 'list number-of-nodes)))
        (cl-ds.utils:fill-distance-matrix-from-vector
         ranges
         (lambda (a b)
           (let ((min nil)
                 (max nil))
             (cl-ds.utils:cartesian
              (list a b)
              (lambda (a b)
                (let ((distance (funcall metric-fn a b)))
                  (if (null min)
                      (setf min distance)
                      (setf min (min distance min)))
                  (if (null max)
                      (setf max distance)
                      (setf max (max distance max))))))
             (list* min max)))
         contents)
        (make 'egnat-node
              :content this-content
              :ranges ranges
              :children children))))
