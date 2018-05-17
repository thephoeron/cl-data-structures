(in-package #:cl-data-structures)


(defun field (name &rest arguments &key &allow-other-keys)
  (lret ((result (make-hash-table :test 'eq)))
    (setf (gethash :name result) name)
    (iterate
      (for argument on arguments)
      (for label = (first argument))
      (for value = (second argument))
      (setf (gethash label result) value))))


(defmethod cl-ds:at ((container cl:hash-table) location &rest more)
  (assert (endp more))
  (gethash location container))
