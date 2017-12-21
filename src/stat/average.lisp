(in-package #:cl-data-structures.statistics)


(defclass average-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric average (range &key key)
  (:generic-function-class accumulate-function)
  (:method (function (range traversable)
            &key (key #'identity) (initial-value nil))
    (apply-aggregation-function range #'average
                                :key key)))


(defstruct average-state
  (count 0 :type non-negative-integer)
  (sum 0 :type number)
  key)


(defmethod cl-ds.alg:state-result ((function average-function)
                                   (state average-state))
  (/ (average-state-sum state)
     (average-state-count state)))


(defmethod cl-ds.alg:make-state ((function average-function)
                                 &rest all
                                 &key
                                   (key #'identity)
                                 &allow-other-keys)
  (declare (ignore all))
  (make-average-state :key key))


(defmethod cl-ds.alg:aggregate ((function average-function)
                                (state average-state)
                                element)
  (incf (average-state-count state))
  (incf (average-state-sum state)
        (funcall (average-state-key state) element)))
