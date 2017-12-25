(in-package #:cl-data-structures.statistics)


(defclass entropy-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric entropy (range &key key base test)
  (:generic-function-class entropy-function)
  (:method (range &key key (base 2) (test 'eql))
    (cl-ds.alg:apply-aggregation-function range
                                          #'entropy
                                          :key key
                                          :base base
                                          :test test)))


(defstruct entropy-state
  (base 2 :type number)
  table
  (count 0 :type fixnum))


(defmethod cl-ds.alg:state-result ((function entropy-function)
                                   state)
  (let ((result 0)
        (base (entropy-state-base state))
        (table (entropy-state-table state))
        (count (entropy-state-count state)))
    (maphash (lambda (key value)
               (declare (ignore key))
               (let ((probability (/ value count)))
                 (incf result (* probability (log probability base)))))
             table)
    (- result)))


(defmethod cl-ds.alg:make-state ((function entropy-function)
                                 &rest all
                                 &key base test
                                 &allow-other-keys)
  (declare (ignore all))
  (make-entropy-state :base base :table (make-hash-table :test test)))


(defmethod cl-ds.alg:aggregate ((function entropy-function)
                                state
                                element)
  (incf (gethash element (entropy-state-table state) 0))
  (incf (entropy-state-count state)))
