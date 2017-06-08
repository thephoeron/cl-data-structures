(in-package #:cl-data-structures.common)


(defclass implementation-modification-operation-status
    (cl-ds:fundamental-modification-operation-status)
  ((%value :initarg :value
           :reader read-value
           :writer write-value)
   (%found :initarg :found
           :reader read-found
           :writer write-value)))


(defclass eager-modification-operation-status
    (implementation-modification-operation-status)
  ())


(defun make-eager-modification-operation-status (found value)
  (make-instance 'eager-modification-operation-status
                 :found found
                 :value value))


(def empty-eager-modification-operation-status
  (make-eager-modification-operation-status nil nil))


(defmethod cl-ds:found ((status eager-modification-operation-status))
  (read-found status))


(defmethod cl-ds:value ((status eager-modification-operation-status))
  (read-value status))
