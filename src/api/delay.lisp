(cl:in-package #:cl-data-structures)


(defclass delayed ()
  ((%callback :initarg :callback
              :reader read-callback)
   %value))


(defun make-delay (callback)
  (make 'delayed :callback callback))


(defmacro delay (&body body)
  `(cl-ds:make-delay (lambda () ,@body)))


(declaim (inline force))
(defun force (obj)
  (if (typep obj 'delayed)
      (if (slot-boundp obj '%value)
          (slot-value obj '%value)
          (setf (slot-value obj '%value)
                (funcall (slot-value obj '%callback))))
      obj))
