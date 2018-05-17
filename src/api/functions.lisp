(in-package #:cl-data-structures)


(defclass field ()
  ((%name :reader name
          :initarg :name)
   (%arguments :initarg :arguments
               :reader read-arguments)))


(defun field (name &rest arguments &key &allow-other-keys)
  (lret ((table (make-hash-table :test 'eq))
         (result (make 'field :name name)))
    (setf (slot-value result '%arguments) table)
    (iterate
      (for argument on arguments)
      (for label = (first argument))
      (for value = (second argument))
      (setf (gethash label table) value))))


(defmethod cl-ds:at ((container field) location &rest more)
  (assert (endp more))
  (gethash location (read-arguments container)))


(defgeneric validate-field (function field))


(defmethod validate-field :around (function field)
  (unless (call-next-method)
    (error "Invalid field members.")))


(defun validate-fields (function fields)
  (map nil (curry #'validate-field function) fields))


(eval-always
  (defun generate-validation-form (body field-name)
    t))


(defmacro define-validation-for-fields (function &body body)
  (with-gensyms (!fn !field)
    `(defmethod validate-field ((!fn ,function) ,!field)
       ,(generate-validation-form body !field))))
