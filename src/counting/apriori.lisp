(in-package cl-data-structures.counting)


(defclass apriori-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric apriori (range minimal-support minimal-frequency &key key parallel)
  (:generic-function-class apriori-function)
  (:method (range minimal-support minimal-frequency &key (key #'identity) (parallel nil))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'apriori
                                               :parallel parallel
                                               :minimal-support
                                               :minimal-frequency
                                               :key key)))


(defun pick-supported (minimal-support minimal-frequency table result)
  (iterate
    (with result = (make-array 32 :adjustable t :fill-pointer 0 :element-type
                               'fixnum))
    (for (key value) in-hashtable table)
    (for count = (second value))
    (when (> count minimal-support)
      (map nil (rcurry #'vector-push-extend result) (third value)))))


(defun apriori-algorithm (minimal-support minimal-frequency vector.table)
  (bind (((vector . table) vector.table)
         (candidates (make-array 32
                                 :adjustable t
                                 :fill-pointer 0 :element-type
                                 'fixnum)))
    (iterate
      (for set in-vector vector)
      (iterate
        (for elt in-vector set)
        (incf (second (gethash (list elt) table)))))
    (pick-supported minimal-support
                    minimal-frequency
                    table
                    candidates)
    (iterate
      (with singular-end = (length candidates))
      (until (emptyp candidates))
      (for super-set = (construct-superset candidates singular-end))
      (for all-subsets = (build-all-subsets super-set)))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function apriori-function)
     &rest all &key parallel minimal-support minimal-frequency key &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :set-form (range &rest all)
          (declare (ignore all))
          (let ((table (make-hash-table :test 'equal))
                (vector (vect)))
            (iterate
              (with i = 0)
              (for j from 0)
              (for (data more) = (cl-ds:consume-front range))
              (while more)
              (for k = (funcall key data))
              (cl-ds:across (lambda (k &aux (k (list k)))
                              (let ((value #1=(gethash k table)))
                                (when (null value)
                                  (setf #1# (list (finc i) 0 (vect j))))))
                            k)
              (vector-push-extend (~> (cl-ds.alg:to-vector
                                       k :key (compose #'first
                                                       (rcurry #'gethash table)
                                                       #'list))
                                      (sort #'<))
                                  vector))
            (cons vector table)))
        (curry #'apriori-algorithm minimal-support minimal-frequency)))


