(in-package #:cl-data-structures.statistics)


(defclass variance-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric variance (range &key key biased)
  (:generic-function-class entropy-function)
  (:method (range &key key (biased t))
    (cl-ds.alg:apply-aggregation-function range
                                          #'variance
                                          :key key
                                          :biased biased)))


(defmethod cl-ds.alg:make-state ((function variance-function)
                                 &rest all &key biased
                                 &allow-other-keys)
  (declare (ignore all))
  (list* (vect) biased))


(defmethod cl-ds.alg:state-result ((function variance-function) state)
  (alexandria:variance (car state) :biased (cdr state)))
