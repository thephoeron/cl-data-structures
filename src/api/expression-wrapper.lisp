(in-package #:cl-data-structures)


(defclass expression (c2mop:funcallable-standard-object
                      fundamental-forward-range)
  ((%construct-function :initarg :construct-function
                        :type function
                        :reader read-body)
   (%arguments :initarg :arguments
               :initform nil
               :reader read-arguments)
   (%closure :accessor access-closure
             :initarg :closure))
  (:metaclass c2mop:funcallable-standard-class))


(defmethod clone ((obj expression))
  (bind (((:slots %construct-function %arguments %closure) obj)
         (result-closure (apply %construct-function (funcall %closure t)))
         (result (make 'expression
                       :construct-function %construct-function
                       :arguments %arguments
                       :closure result-closure)))
    result))


(defmethod initialize-instance :after ((obj expression) &rest all
                                       &key &allow-other-keys)
  (declare (ignore all))
  (if (slot-boundp obj '%closure)
      (let ((function (access-closure obj)))
        (c2mop:set-funcallable-instance-function obj
                                                 (lambda () (funcall function))))
      (reset! obj)))


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
  (declare (optimize (speed 3) (debug 0) (space 0)))
  (let ((fn (access-closure obj))
        (function (ensure-function function)))
    (declare (type (-> (t) t) function)
             (type (-> (&optional boolean) t) fn))
    (iterate
      (for (values value not-finished) = (funcall fn))
      (while not-finished)
      (funcall function value)))
  obj)


(defmethod across (function (obj expression))
  (declare (optimize (speed 3) (debug 0) (space 0)))
  (let ((function (ensure-function function)))
    (declare (type (-> (t) t) function))
    (bind (((:slots %construct-function %closure) obj)
           (fn (apply %construct-function (funcall %closure t))))
      (declare (type (-> (&optional boolean) t) fn))
      (iterate
        (for (values value not-finished) = (funcall fn))
        (while not-finished)
        (funcall function value))
      obj)))


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


(defmethod drop-front ((obj expression) count)
  (check-type count non-negative-fixnum)
  (let ((function (access-closure obj)))
    (iterate
      (repeat count)
      (for i from 0)
      (for (values value more) = (funcall function))
      (finally (return i)))))
