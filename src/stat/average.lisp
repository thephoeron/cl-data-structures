(in-package #:cl-data-structures.statistics)


(defclass average-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric average (range &key key)
  (:generic-function-class average-function)
  (:method (range
            &key key)
    (cl-ds.alg:apply-aggregation-function range #'average
                                          :key key)))


(defmethod cl-ds.alg:state-result ((function average-function)
                                   state)
  (declare (type list state))
  (unless (zerop (cdr state))
    (/ (car state) (cdr state))))


(defmethod cl-ds.alg:make-state ((function average-function)
                                 &rest all
                                 &key
                                 &allow-other-keys)
  (declare (ignore all))
  (list* 0 0))


(defmethod cl-ds.alg:aggregate ((function average-function)
                                state
                                element)
  (declare (type list state))
  (incf (cdr state))
  (incf (car state) element))
