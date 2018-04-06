(in-package #:cl-data-structures.math)


(defclass variance-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric variance (range &key key biased)
  (:generic-function-class variance-function)
  (:method (range &key key (biased t))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'variance
                                               :key key
                                               :biased biased)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn variance-function)
                                                    &rest all
                                                    &key key
                                                    &allow-other-keys)
  (declare (ignore all))
  `((:average . ,(lambda (range)
                   (average range :key key)))))


(defstruct variance-state average (sum 0) (count 0 :type fixnum))


(defmethod cl-ds.alg.meta:make-state ((function variance-function)
                                      &rest all &key biased average
                                      &allow-other-keys)
  (declare (ignore all))
  (make-variance-state :average average :count (if biased 0 -1)))


(defmethod cl-ds.alg.meta:state-result ((function variance-function) state)
  (/ (variance-state-sum state)
     (variance-state-count state)))


(defmethod cl-ds.alg.meta:aggregate ((function variance-function) state element)
  (incf (variance-state-count state))
  (incf (variance-state-sum state) (expt (- element (variance-state-average state)) 2)))
