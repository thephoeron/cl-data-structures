(in-package #:cl-data-structures)


(defclass accumulate-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric accumulate (function range &key key initial-value)
  (:generic-function-class accumulate-function)
  (:method (function (range fundamental-range) &key (key #'identity) (initial-value nil))
    (apply-aggregation-function range #'accumulate
                                :key key
                                :value initial-value
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
  (access-value state))


(defmethod make-state ((function accumulate-function)
                       &rest all
                       &key
                         (initial-value nil value-present)
                         fn
                         key
                       &allow-other-keys)
  (declare (ignore all))
  (if value-present
      (make 'accumulation-state :fn fn
                                :key key
                                :value initial-value)
      (make 'accumulation-state :fn fn
                                :key key)))


(defmethod aggregate ((function accumulate-function)
                      (state accumulation-state)
                      element)
  (with-slots ((value %value) (fn %fn) (key %key)) state
    (setf value
          (if (slot-boundp state '%value)
              (funcall fn
                       value
                       (funcall key element))
              (funcall key element)))))
