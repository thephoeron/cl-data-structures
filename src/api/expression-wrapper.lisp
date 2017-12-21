(in-package #:cl-data-structures)


(defclass expression (traversable)
  ((%body :initarg :body
          :type (-> ((or function symbol)) t)
          :reader read-body)))


(defmacro xpr (&body body)
  (with-gensyms (!fn)
    `(make 'cl-ds:expression
           :body (lambda (,!fn)
                   (macrolet ((cl-ds:send (v)
                                `(funcall ,',!fn ,v)))
                     ,@body)))))


(defmethod cl-ds:traverse (function (obj expression))
  (funcall (read-body obj) function)
  (slot-makunbound obj '%body)
  obj)
