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
  ((%close-range
    :type (or null cl-ds.utils:distance-matrix)
    :initform nil
    :initarg :close-range
    :reader read-close-range)
   (%distant-range
    :type (or null cl-ds.utils:distance-matrix)
    :initarg :distant-range
    :initform nil
    :reader read-distant-range)
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
      (map nil #'force-tree %children)))
  node)


(defun make-future-egnat-nodes (data number-of-nodes content-count metric-fn metric-type)
  (if (<= (length data) content-count)
      (make 'egnat-node :content data)
      (bind ((this-content (take content-count data))
             (this-data (drop (length this-content) data))
             (seeds-indexes (iterate
                              (with generator = (cl-ds.utils:lazy-shuffle 0 (length this-data)))
                              (with result = (make-array number-of-nodes
                                                         :element-type 'fixnum
                                                         :fill-pointer 0
                                                         :adjustable t))
                              (for i = (funcall generator))
                              (repeat number-of-nodes)
                              (while i)
                              (vector-push-extend i result)
                              (finally (return result))))
             (reverse-mapping (let ((table (make-hash-table :size (length seeds-indexes))))
                                (iterate
                                  (for i in-vector seeds-indexes)
                                  (for j from 0)
                                  (setf (gethash i table) j))
                                table))
             (data-partitions (map '(vector fixnum)
                                   (lambda (value)
                                     (iterate
                                       (for seed-index in-vector seeds-indexes)
                                       (for seed = (aref this-data seed-index))
                                       (for distance = (funcall metric-fn value seed))
                                       (minimize distance into min)
                                       (finding seed-index such-that (= distance min))))
                                   this-data))
             (contents (let ((result (make-array number-of-nodes
                                                 :adjustable t
                                                 :fill-pointer (length seeds-indexes))))
                         (map-into result #'vect)
                         (iterate
                           (for d in-vector data-partitions)
                           (for i from 0)
                           (for element = (aref this-data i))
                           (for partition = (gethash d reverse-mapping))
                           (vector-push-extend element (aref result partition)))
                         (remove-if #'emptyp result)))
             (children (map-into
                        (make-array number-of-nodes
                                    :adjustable t
                                    :fill-pointer (length contents))
                        (lambda (content)
                          (lparallel:future
                            (make-future-egnat-nodes content number-of-nodes
                                                     content-count metric-fn metric-type)))
                        contents))
             (ranges (cl-ds.utils:make-distance-matrix 'list number-of-nodes))
             (close-range (cl-ds.utils:make-distance-matrix metric-type number-of-nodes))
             (distant-range (cl-ds.utils:make-distance-matrix metric-type number-of-nodes)))
        (cl-ds.utils:fill-distance-matrix-from-vector
         ranges
         (lambda (a b)
           (let ((min nil)
                 (max nil))
             (cl-ds.utils:cartesian
              (list a b)
              (lambda (a b)
                (let ((distance (coerce (funcall metric-fn a b)
                                        metric-type)))
                  (if (null min)
                      (setf min distance)
                      (setf min (min distance min)))
                  (if (null max)
                      (setf max distance)
                      (setf max (max distance max))))))
             (list* min max)))
         contents)
        (cl-ds.utils:mutate-matrix close-range
                                   (lambda (x)
                                     (if (listp x)
                                         (car x)
                                         (coerce 0 metric-type)))
                                   ranges)
        (cl-ds.utils:mutate-matrix distant-range
                                   (lambda (x)
                                     (if (listp x)
                                         (cdr x)
                                         (coerce 0 metric-type)))
                                   ranges)
        (make 'egnat-node
              :content this-content
              :close-range close-range
              :distant-range distant-range
              :children children))))


(defun make-egnat-tree (data number-of-nodes content-count metric-fn metric-type)
  (~> data
      (make-future-egnat-nodes number-of-nodes content-count metric-fn metric-type)
      force-tree))
