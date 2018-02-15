(in-package #:cl-data-structures)


(defclass expression (c2mop:funcallable-standard-object
                      fundamental-forward-range)
  ((%construct-function :initarg :construct-function
                        :type (-> () function)
                        :reader read-body)
   (%arguments :initarg :arguments
               :initform nil
               :reader read-arguments)
   (%closure :accessor access-closure))
  (:metaclass c2mop:funcallable-standard-class))


(defmethod initialize-instance :after ((obj expression) &rest all
                                       &key &allow-other-keys)
  (declare (ignore all))
  (reset! obj))


(defmacro xpr (arguments &body body)
  (let ((keys (plist-keys arguments)))
    (with-gensyms (!fn)
      `(cl-ds.utils:let-generator
           ((,!fn ,(mapcar (lambda (x) (intern (symbol-name x))) keys)
                  ,@body))
         (make 'cl-ds:expression
               :construct-function (function ,!fn)
               :arguments (list ,@arguments))))))


(defmethod traverse (function (obj expression))
  (iterate
    (for (values value not-finished) = (funcall obj))
    (while not-finished)
    (funcall function value))
  obj)


(defmethod across (function (obj expression))
  (bind (((:slots %arguments %construct-function %closure) obj)
         (fn (apply %construct-function (funcall %closure t))))
    (iterate
      (for (values value not-finished) = (funcall fn))
      (while not-finished)
      (funcall function value))
    obj))


(defmethod consume-front ((obj expression))
  (funcall obj))


(defmethod peek-front ((obj expression))
  (bind (((:slots %closure %construct-function) obj)
         (fn (apply %construct-function (funcall %closure t))))
    (funcall fn)))


(defmethod reset! ((obj expression))
  (bind (((:slots %construct-function %arguments %closure) obj)
         (function (apply %construct-function
                          %arguments)))
    (setf %closure function)
    (c2mop:set-funcallable-instance-function obj (lambda () (funcall function))))
  obj)
