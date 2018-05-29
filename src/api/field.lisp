(in-package #:cl-data-structures)


(defclass field ()
  ((%arguments :initarg :arguments
               :reader read-arguments)))


(defun field (&rest arguments &key &allow-other-keys)
  (lret ((table (make-hash-table :test 'eq))
         (result (make 'field)))
    (setf (slot-value result '%arguments) table)
    (iterate
      (for argument
           initially arguments
           then (cddr argument))
      (until (endp argument))
      (for label = (first argument))
      (for value = (second argument))
      (for (values v found) = (gethash label table))
      (when found
        (error 'invalid-argument
               :argument label
               :text "Can't set value in field twice!"))
      (setf (gethash label table) value))))


(defmethod cl-ds:at ((container field) location &rest more)
  (assert (endp more))
  (gethash location (read-arguments container)))


(defmethod (setf cl-ds:at) (value (container field) location &rest more)
  (assert (endp more))
  (setf (gethash location (read-arguments container)) value))


(defgeneric validate-field (function field))


(defmethod validate-field :around (function field)
  (unless (call-next-method)
    (error 'invalid-argument
           :text "Validation of field failed."
           :argument (hash-table-alist (read-arguments field)))))


(defun validate-fields (function fields)
  (map nil (curry #'validate-field function) fields))


(eval-always
  (defgeneric validation-form-for (parameter-name argument-name
                                   field-name value-name
                                   value-found parameter-body))


  (defmethod validation-form-for ((parameter-name (eql :optional))
                                  argument-name field-name
                                  value-name value-found body)
    `(econd
       ((eq ,body t)
        (unless ,value-found
          (return-from nil t)))
       ((eq ,body nil)
        (unless ,value-found
          (return-from nil nil)))))


  (defmethod validation-form-for ((parameter-name (eql :member))
                                  argument-name field-name
                                  value-name value-found body)
    `(unless (or (not ,value-found) (member ,value-name '(,@body)))
       (return-from nil nil)))


  (defmethod validation-form-for ((parameter-name (eql :default))
                                  argument-name field-name
                                  value-name value-found body)
    `(unless ,value-found
       (setf (cl-ds:at ,field-name ,argument-name) ,body)))


  (defmethod validation-form-for ((parameter-name (eql :type))
                                  argument-name field-name
                                  value-name value-found body)
    `(when ,value-found
       (typep ,value-name ,body)))


  (defun validation-form (parameters-list argument-name field-name)
    (with-gensyms (!value !found)
      `(bind (((:values ,!value ,!found) (cl-ds:at ,field-name ,argument-name)))
         (declare (ignorable ,!value ,!found))
         ,@(mapcar (lambda (x &aux
                           (parameter-name (car x))
                           (parameter-body (cdr x)))
                     (validation-form-for parameter-name argument-name
                                          field-name !value !found parameter-body))
                   parameters-list))))


  (defun generate-validation-forms (body field-name accepted-field-arguments)
    (let ((ordering (ordering accepted-field-arguments))
          (error-text (format
                       nil
                       "Field does not accept this argument. Accepted arguments are: ~{~A~^, ~}."
                       accepted-field-arguments)))
      (with-gensyms (!key !arg)
        `(block nil
           (iterate
             (for (,!key ,!arg) in-hashtable (read-arguments ,field-name))
             (unless (member ,!key '(,@accepted-field-arguments))
               (error 'cl-ds:unexpected-argument
                      :argument ,!key
                      :text ,error-text)))
           ,@(mapcar (lambda (x)
                       (bind (((name . parameters-list) x))
                         (~> (plist-alist parameters-list)
                             (sort ordering :key #'first)
                             (validation-form name field-name))))
                     body)
           t)))))


(defmacro define-validation-for-fields ((fn accepted-field-arguments) &body body)
  (with-gensyms (!fn !field)
    `(defmethod validate-field ((,!fn ,fn) ,!field)
       ,(generate-validation-forms body !field accepted-field-arguments))))
