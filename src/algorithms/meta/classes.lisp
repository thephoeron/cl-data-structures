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


(defclass fundamental-aggregation-stage ()
  ((%key :initarg :key
         :initform #'identity
         :reader read-key)))


(defclass aggregation-stage (fundamental-aggregation-stage)
  ((%name :initarg :name
          :reader read-name)
   (%construct-function :reader read-construct-function
                        :initarg :construct-function)
   (%function :initarg :function
              :reader read-function)
   (%state :reader read-state
           :initarg :state)))


(defclass reduce-stage (fundamental-aggregation-stage)
  ((%name :initarg :name
          :reader read-name)
   (%function :initarg :function
              :reader read-function)
   (%state :accessor access-state
           :initarg :state)))


(defclass linear-aggregator (fundamental-aggregator)
  ((%function :initarg :function
              :reader read-function)
   (%state :initarg :state
           :accessor read-state)
   (%ended :initform nil
           :accessor access-ended)))


(defclass multi-aggregator (fundamental-aggregator)
  ())


(defclass multi-stage-linear-aggregator (multi-aggregator)
  ((%stages :initarg :stages
            :accessor access-stages)
   (%accumulator :initform nil
                 :accessor access-accumulator)))


(defun %stage (name construct-function)
  (make 'aggregation-stage
        :name name
        :construct-function construct-function))


(defun %reduce-stage (name state-init function)
  (make 'reduce-stage
        :name name
        :function function
        :state state-init))


(defmacro stage (name lambda-list &body body)
  `(%stage ,name (lambda ,lambda-list ,@body)))


(defmacro reduce-stage (name init-form lambda-list &body body)
  `(%reduce-stage ,name ,init-form (lambda ,lambda-list ,@body)))


(defun make-linear-aggregator (function arguments key)
  (make 'linear-aggregator
        :key key
        :function function
        :arguments arguments))


(defun make-multi-stage-linear-aggregator (arguments key stages)
  (make 'multi-stage-linear-aggregator
        :key key
        :stages stages
        :arguments arguments))
