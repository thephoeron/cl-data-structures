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
   (%size
    :reader read-size
    :type non-negative-fixnum
    :initform 0)
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


(defun make-future-egnat-nodes (operation container extra-arguments data)
  (bind (((:slots %metric-fn %metric-type %content-count-in-node %branching-factor) container)
         (length (length data)))
    (if (<= length %content-count-in-node)
        (make 'egnat-node :content (apply #'cl-ds:make-bucket
                                          operation
                                          container
                                          data
                                          extra-arguments))
        (bind ((this-content (make-array %content-count-in-node :displaced-to data))
               (this-data (make-array (- length %content-count-in-node)
                                      :displaced-to data
                                      :displaced-index-offset %content-count-in-node))
               (seeds-indexes (iterate
                                (with generator = (cl-ds.utils:lazy-shuffle 0 (length this-data)))
                                (with result = (make-array %branching-factor
                                                           :element-type 'fixnum
                                                           :fill-pointer 0
                                                           :adjustable t))
                                (for i = (funcall generator))
                                (repeat %branching-factor)
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
                                         (for distance = (funcall %metric-fn value seed))
                                         (minimize distance into min)
                                         (finding seed-index such-that (= distance min))))
                                     this-data))
               (contents (let ((result (make-array %branching-factor
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
                          (make-array %branching-factor
                                      :adjustable t
                                      :fill-pointer (length contents))
                          (lambda (content)
                            (lparallel:future
                              (make-future-egnat-nodes operation container extra-arguments content)))
                          contents))
               (ranges (cl-ds.utils:make-distance-matrix 'list %branching-factor))
               (close-range (cl-ds.utils:make-distance-matrix %metric-type %branching-factor))
               (distant-range (cl-ds.utils:make-distance-matrix %metric-type %branching-factor)))
          (cl-ds.utils:fill-distance-matrix-from-vector
           ranges
           (lambda (a b)
             (let ((min nil)
                   (max nil))
               (cl-ds.utils:cartesian
                (list a b)
                (lambda (a b)
                  (let ((distance (coerce (funcall %metric-fn a b)
                                          %metric-type)))
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
                                           (coerce 0 %metric-type)))
                                     ranges)
          (cl-ds.utils:mutate-matrix distant-range
                                     (lambda (x)
                                       (if (listp x)
                                           (cdr x)
                                           (coerce 0 %metric-type)))
                                     ranges)
          (make 'egnat-node
                :content (apply #'cl-ds:make-bucket
                                operation
                                container
                                this-content
                                extra-arguments)
                :close-range close-range
                :distant-range distant-range
                :children children)))))


(defun make-egnat-tree (operation container extra-arguments data)
  (~>> data
       (make-future-egnat-nodes operation container extra-arguments)
       force-tree))


(defun prune-subtrees (trees close-range distant-range value metric-fn)
  (let ((result (make-array (length trees)
                            :element-type 'bit
                            :initial-element 1))
        (length (length trees)))
    (iterate
      (for i from 0)
      (for tree in-vector trees)
      (for content = (~> tree read-content (aref 0)))
      (for distance = (funcall metric-fn value content))
      (iterate
        (for j from 0 below length)
        (unless (or (eql i j) (zerop (aref result j)))
          (let ((in-range (<= (cl-ds.utils:distance close-range i j)
                              distance
                              (cl-ds.utils:distance distant-range i j))))
            (setf (aref result j) (if in-range 1 0))))))
    result))


(defun insert-into-node (operation container node item extra-arguments)
  (bind (((:slots %content) node))
    (apply #'cl-ds:grow-bucket!
           operation
           container
           node
           item
           extra-arguments)))


(defun node-full (container node)
  (cl-ds:full-bucket-p container (read-content node)))
