(in-package #:cl-data-structures.algorithms.meta)


(defclass range-function (closer-mop:standard-generic-function)
  ())


(defclass layer-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass multi-aggregation-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass fundamental-aggregator ()
  ((%arguments :initarg :arguments
               :accessor access-arguments
               :initform nil)
   (%key :initarg :key
         :reader read-key
         :initform #'identity)))


(defclass abstract-proxy-aggregator (fundamental-aggregator)
  ((%inner-aggregator :initarg :inner-aggregator
                      :reader read-inner-aggregator)))


(defmethod cl-ds:forward-call ((object abstract-proxy-aggregator)
                               function)
  (funcall function (read-inner-aggregator object)))


(defclass linear-aggregator (fundamental-aggregator)
  ((%function :initarg :function
              :reader read-function)
   (%state :initarg :state
           :accessor read-state)
   (%ended :initform nil
           :accessor access-ended)))


(defun make-linear-aggregator (function arguments key)
  (make 'linear-aggregator
        :key key
        :state (apply #'make-state function arguments)
        :function function
        :arguments arguments))
