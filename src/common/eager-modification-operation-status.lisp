(in-package #:cl-data-structures.common)


(defclass eager-modification-operation-status
    (implementation-modification-operation-status)
  ())


(defun make-eager-modification-operation-status (found value changed)
  (make-instance 'eager-modification-operation-status
                 :found found
                 :changed changed
                 :value value))


(def empty-eager-modification-operation-status
  (make-eager-modification-operation-status nil nil nil))


(def empty-changed-eager-modification-operation-status
  (make-eager-modification-operation-status nil nil t))
