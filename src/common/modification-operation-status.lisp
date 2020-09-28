(in-package :cl-data-structures.common)

(defclass implementation-modification-operation-status (cl-ds:fundamental-modification-operation-status)
  ((%value :initarg :value :reader read-value :reader cl-ds:value :writer write-value)
   (%changed :initarg :changed :reader cl-ds:changed :writer write-changed :reader read-changed)
   (%found :initarg :found :reader read-found :reader cl-ds:found :writer write-found))
  (:metaclass funcallable-standard-class))
