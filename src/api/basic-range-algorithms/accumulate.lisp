(in-package #:cl-data-structures)


(defclass accumulate-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric accumulate (function range &key key initial-value)
  (:generic-function-class accumulate-function)
  (:method (function (range fundamental-range) &key (key #'identity) (initial-value nil))
    (apply-aggregation-function range #'accumulate
                                :key key
                                :initial-value initial-value
                                :fn function)))


(defclass accumulation-state ()
  ((%value :initarg :value
           :accessor access-value)
   (%fn :initarg :fn
        :reader read-fn)
   (%key :initarg :key
         :reader read-key)))


(defmethod state-result ((function accumulate-function)
                         (state accumulation-state))
  (read-value state))


(defmethod make-state ((function accumulate-function)
                       &rest all
                       &key
                         (initial-value nil value-present)
                         fn
                       &allow-other-keys)
  (apply #'make-instance 'accumulation-state all))


(defmethod aggregate ((function accumulate-function)
                      (state accumulation-state)
                      element)
  (with-slots ((value %value) (fn %fn) (key %key)) state
    (if (slot-boundp state '%value)
        (setf value (funcall key element))
        (setf value
              (funcall fn
                       value
                       (funcall key element))))))
