(in-package #:cl-data-structures.algorithms)


(defclass accumulate-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric accumulate (function range &key key initial-value)
  (:generic-function-class accumulate-function)
  (:method (function range
            &key (key #'identity) (initial-value nil initial-value-present))
    (if initial-value-present
        (apply-aggregation-function range #'accumulate
                                    :key key
                                    :initial-value initial-value
                                    :fn function)
        (apply-aggregation-function range #'accumulate
                                    :key key
                                    :fn function))))


(defclass accumulation-state ()
  ((%value :initarg :value
           :accessor access-value)
   (%fn :initarg :fn
        :reader read-fn)
   (%first-iteration :initform t
                     :accessor access-first-iteration)))


(defmethod state-result ((function accumulate-function)
                         (state accumulation-state))
  (if (access-first-iteration state)
      (funcall (read-fn state) (access-value state))
      (access-value state)))


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
  (with-slots ((value %value) (first-iteration %first-iteration) (fn %fn)) state
    (setf value
          (if (slot-boundp state '%value)
              (progn
                (setf first-iteration nil)
                (funcall fn
                         value
                         element))
              element))))
