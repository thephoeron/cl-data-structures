(in-package #:cl-data-structures)


(defclass delayed ()
  ((%callback :initarg :callback
              :reader read-callback)
   %value))


(defgeneric force (obj)
  (:method ((obj delayed))
    (if (slot-boundp obj '%value)
        (slot-value obj '%value)
        (setf (slot-value obj '%value)
              (funcall (read-callback obj)))))
  (:method ((obj t))
    (slot-value obj '%value)))
