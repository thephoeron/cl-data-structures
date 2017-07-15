(in-package #:cl-data-structures.common)


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
