(in-package #:cl-data-structures)


(defclass field ()
  ((%name :reader name
          :initarg :name)
   (%arguments :initarg :arguments
               :reader read-arguments)))


(defun field (name &rest arguments &key &allow-other-keys)
  (lret ((table (make-hash-table :test 'eq))
         (result (make 'field :name name)))
    (setf (slot-value table '%arguments) table)
    (iterate
      (for argument on arguments)
      (for label = (first argument))
      (for value = (second argument))
      (setf (gethash label result) value))))


(defmethod cl-ds:at ((container cl:hash-table) location &rest more)
  (assert (endp more))
  (gethash location (read-arguments container)))

