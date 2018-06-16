(in-package cl-data-structures.counting)


(defclass apriori-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric apriori (range minimal-support minimal-frequency
                     &key key parallel)
  (:generic-function-class apriori-function)
  (:method (range minimal-support minimal-frequency
            &key (key #'identity) (parallel nil))
    (cl-ds.alg.meta:apply-aggregation-function
     range
     #'apriori
     :parallel parallel
     :minimal-support minimal-support
     :minimal-frequency minimal-frequency
     :key key)))


(defclass apriori-node ()
  ((%type :reader read-type
          :initarg :type
          :initform nil
          :type (or null integer))
   (%locations :reader read-locations
               :initarg :locations
               :type (vector fixnum))
   (%count :reader read-count
           :initarg :count
           :type integer)
   (%sets :reader read-sets
          :writer write-sets
          :type vector
          :initarg :sets
          :initform (vect))
   (%parent :initarg :parent
            :initform nil
            :writer write-parent
            :reader read-parent)))


(defmethod initialize-instance :after ((node apriori-node)
                                       &key &allow-other-keys)
  (when (slot-boundp node '%locations)
    (setf (slot-value node '%count)
          (length (read-locations node)))))


(defun sort-sets (node)
  (write-sets (sort (read-sets node) #'< :key #'read-type)
              node)
  node)


(defclass apriori-index ()
  ((%root :reader read-root
          :initarg :root)
   (%minimal-support :reader read-minimal-support
                     :initarg :minimal-support)
   (%minimal-frequency :reader read-minimal-frequency
                       :initarg :minimal-frequency)
   (%total-size :reader read-total-size
                :initarg :total-size)))


(defun make-apriori-index (table total-size minimal-support minimal-frequency)
  (let ((root-content (make-array (hash-table-count table))))
    (iterate
      (for i from 0)
      (for (key value) in-hashtable table)
      (for (id . positions) = value)
      (setf (aref root-content i) (make 'apriori-node
                                        :type key
                                        :locations positions)))
    (let ((result (make
                   'apriori-index
                   :total-size total-size
                   :minimal-support minimal-support
                   :minimal-frequency minimal-frequency
                   :root (~> root-content
                             (delete-if
                              (lambda (x &aux (length (read-count x)))
                                (or (< length minimal-support)
                                    (< (/ length total-size)
                                       minimal-frequency)))
                              _)
                             (sort #'< :key #'read-type)
                             (make-instance 'apriori-node
                                            :sets _
                                            :count total-size)))))
      (map nil
           (curry #'write-parent (read-root result))
           (~> result read-root read-sets))
      result)))


(defun combine-nodes (node)
  (let* ((last-elt node)
         (parent (read-parent last-elt))
         (content (read-sets parent))
         (lower-bound (lower-bound content
                                   (read-type last-elt)
                                   #'<
                                   :key #'read-type)))
    (iterate
      (for i from (1+ lower-bound) below (length content))
      (for elt = (aref content i))
      (collect elt at start))))


(defun construct-supersets (index node)
  (if (null (~> node read-parent))
      (iterate
        (with root = (~> index read-root read-sets))
        (with size = (length root))
        (with result = nil)
        (for i from 0 below size)
        (for elt = (aref root i))
        (iterate
          (for j from i below size)
          (for elt2 = (aref root j))
          (unless (eql (read-type elt) (read-type elt2))
            (push (lret ((array (make-array 2)))
                    (setf (aref array 1) elt2
                          (aref array 0) elt))
                  result)))
        (finally (return result)))
      (combine-nodes node)))


(defun number-of-children (node)
  (~> node read-sets length))


(defun children-at (node i)
  (~> node read-sets (aref i)))


(defun push-children (parent children)
  (vector-push-extend children (read-sets parent)))


(lparallel:defpun expand-node (index parent i)
  (when (< i (number-of-children parent))
    (expand-node index parent (1+ i))
    (let* ((node (children-at parent i))
           (supersets (construct-supersets index node)))
      (iterate
        (for superset in supersets)
        (for intersection = (ordered-intersection
                             #'< #'eql
                             (read-locations node)
                             (read-locations superset)))
        (for intersection-size = (length intersection))
        (when (< intersection-size (read-minimal-support index))
          (next-iteration))
        (for new-node = (make 'apriori-node
                              :locations intersection
                              :parent node
                              :type (read-type superset)))
        (push-children node new-node))
      (sort-sets node)
      (expand-node index node 0))))


(defun apriori-algorithm (&key set-form minimal-support
                            minimal-frequency &allow-other-keys)
  (declare (optimize (debug 3)))
  (bind (((_ total-size . table) set-form)
         (index (make-apriori-index table
                                    total-size
                                    minimal-support
                                    minimal-frequency)))
    (expand-node index (read-root index) 0)
    index))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function apriori-function)
     &rest all
     &key minimal-support minimal-frequency key &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:reduce-stage :set-form
            (list* -1 0 (make-hash-table :test 'equal))
            (state data &rest all)
          (declare (ignore all))
          (bind (((_ position . table) state)
                 (k (funcall key data)))
            (cl-ds:across (lambda (k)
                            (ensure (gethash k table) (list* (incf (car state))
                                                             (make-array 16 :element-type 'fixnum
                                                                            :adjustable t
                                                                            :fill-pointer 0)))
                            (vector-push-extend position (cdr (gethash k table))))
                          k))
          (incf (second state))
          state)
        (curry #'apriori-algorithm minimal-support minimal-frequency)))
