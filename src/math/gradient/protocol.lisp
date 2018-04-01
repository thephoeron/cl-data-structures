(in-package #:cl-data-structures.math.gradient)


(defgeneric tape-backward (operation-symbol &rest all))
(defgeneric tape-backward-form (operation-symbol established-bindings))


(defmacro define-tape-backward (operation-symbol argument &body body)
  (with-gensyms (!bindings)
    `(progn
       (defmethod tape-backward ((operation-symbol (eql ',operation-symbol))
                                 &rest ,argument)
         ,@body)
       (defmethod tape-backward-form ((operation-symbol (eql ',operation-symbol))
                                      ,!bindings)
         (flet ((form (bindings)
                  `(let ((,',argument (list ,@bindings)))
                     (declare (dynamic-extent ,',argument))
                     ,',@body)))
           (form ,!bindings))))))


(defgeneric compile-gradient-expression (gradient-expression))
