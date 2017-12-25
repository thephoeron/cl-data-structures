(in-package #:cl-data-structures.statistics)


(defclass average-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric average (range &key key)
  (:generic-function-class average-function)
  (:method ((range cl-ds:traversable)
            &key (key #'identity) )
    (cl-ds.alg:apply-aggregation-function range #'average
                                          :key key)))


(defstruct average-state
  (count 0 :type non-negative-integer)
  (sum 0 :type number))


(defmethod cl-ds.alg:state-result ((function average-function)
                                   (state average-state))
  (unless (zerop (average-state-count state))
    (/ (average-state-sum state)
       (average-state-count state))))


(defmethod cl-ds.alg:make-state ((function average-function)
                                 &rest all
                                 &key
                                 &allow-other-keys)
  (declare (ignore all))
  (make-average-state))


(defmethod cl-ds.alg:aggregate ((function average-function)
                                (state average-state)
                                element)
  (incf (average-state-count state))
  (incf (average-state-sum state)
        element))
