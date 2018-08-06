(in-package #:cl-data-structures.file-system)


(defclass file-range-mixin ()
  ((%stream :initarg :stream
            :initform nil
            :type (or stream null)
            :reader read-stream)))


(defgeneric close-inner-stream (range)
  (:method ((range file-range-mixin))
    (when-let ((stream (read-stream range)))
      (close stream)
      (setf (slot-value range '%stream) nil))
    range))


(defmacro with-file-ranges (bindings &body body)
  (with-gensyms (!tmp)
    `(let ,(mapcar (lambda (x) (list (first x) nil)) bindings)
       (unwind-protect
            (progn
              ,@(mapcar (lambda (x) `(setf ,(first x)
                                      (lret ((,!tmp ,(second x)))
                                        (check-type ,!tmp file-range-mixin))))
                        bindings)
              ,@body)
         (progn
           ,@(mapcar (lambda (x) `(unless (null ,(first x))
                               (close-inner-stream ,(first x))))
                     bindings))))))


(flet ((enclose-finalizer (stream)
         (lambda () (when stream (close stream)))))
  (defmethod initialize-instance :after ((range file-range-mixin)
                                         &rest all)
    (declare (ignore all))
    (trivial-garbage:finalize range (enclose-finalizer (read-stream range)))))
