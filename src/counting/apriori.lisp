(in-package cl-data-structures.counting)


(defclass apriori-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric apriori (range minimal-support &key key)
  (:generic-function-class apriori-function)
  (:method (range minimal-support &key (key #'identity))
    (ensure-functionf key)
    (check-type minimal-support positive-fixnum)
    (cl-ds.alg.meta:apply-aggregation-function
     range
     #'apriori
     :minimal-support minimal-support
     :key key)))


(defun ensure-all-two-level-deep-nodes (index)
  (bind ((root (read-root index))
         (content (read-sets root)))
    (iterate
      (for i from 1 below (length content))
      (for node = (aref content i))
      (iterate
        (for j from 0 below i)
        (for alternative-node = (aref content j))
        (for symetric-node = (node-at index
                                      (read-type alternative-node)
                                      (read-type node)))
        (when (null symetric-node) (next-iteration))
        (push-child node (make 'apriori-node
                               :type (read-type alternative-node)
                               :locations (read-locations symetric-node))))
      (sort-sets node))))


(defun apriori-algorithm (&key set-form minimal-support &allow-other-keys)
  (bind (((_ total-size . table) set-form)
         (index (make-apriori-index table
                                    total-size
                                    minimal-support))
         (queue (lparallel.queue:make-queue)))
    (async-expand-node index (read-root index) 0 queue)
    (let ((reverse-mapping (make-array (hash-table-count table)))
          (mapping (make-hash-table :size (hash-table-count table)
                                    :test 'equal)))
      (iterate
        (for i from 0)
        (for (key value) in-hashtable table)
        (for (id . positions) = value)
        (setf (aref reverse-mapping id) key
              (gethash key mapping) i))
      (setf (access-mapping index) mapping
            (access-reverse-mapping index) reverse-mapping)
      (iterate
        (for (values f more) = (lparallel.queue:try-pop-queue queue))
        (while more)
        (lparallel:force f))
      (ensure-all-two-level-deep-nodes index)
      (reset-locations index)
      index)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function apriori-function)
     &rest all
     &key minimal-support key &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:reduce-stage :set-form
            (list* -1 0 (make-hash-table :test 'equal))
            (state data &rest all)
          (declare (ignore all))
          (bind (((_ position . table) state))
            (cl-ds:across (lambda (k)
                            (ensure (gethash k table)
                              (list* (incf (car state))
                                     (make-array 4 :element-type 'fixnum
                                                   :adjustable t
                                                   :fill-pointer 0)))
                            (vector-push-extend position
                                                (cdr (gethash k table))))
                          (funcall key data)))
          (incf (second state))
          state)
        #'apriori-algorithm))
