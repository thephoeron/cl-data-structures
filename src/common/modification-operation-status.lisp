(in-package #:cl-data-structures.common)


(defclass implementation-modification-operation-status
    (cl-ds:fundamental-modification-operation-status)
  ((%value :initarg :value
           :reader read-value
           :writer write-value)
   (%found :initarg :found
           :reader read-found
           :writer write-found)))
