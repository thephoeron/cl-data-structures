(in-package #:cl-data-structures.algorithms)


(defclass accumulate-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric accumulate (function range &key key initial-value)
  (:generic-function-class accumulate-function)
  (:method (function (range traversable)
            &key (key #'identity) (initial-value nil))
    (apply-aggregation-function range #'accumulate
                                :key key
                                :value initial-value
                                :fn function)))


(defclass accumulation-state ()
  ((%value :initarg :value
           :accessor access-value)
   (%fn :initarg :fn
        :reader read-fn)))


(defmethod state-result ((function accumulate-function)
                         (state accumulation-state))
  (access-value state))


(defmethod make-state ((function accumulate-function)
                       &rest all
                       &key
                         (initial-value nil value-present)
                         fn
                       &allow-other-keys)
  (declare (ignore all))
  (if value-present
      (make 'accumulation-state :fn fn
                                :value initial-value)
      (make 'accumulation-state :fn fn)))


(defmethod aggregate ((function accumulate-function)
                      (state accumulation-state)
                      element)
  (with-slots ((value %value) (fn %fn)) state
    (setf value
          (if (slot-boundp state '%value)
              (funcall fn
                       value
                       element)
              element))))
