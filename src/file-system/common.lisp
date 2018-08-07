(in-package #:cl-data-structures.file-system)


(defclass file-range-mixin ()
  ((%stream :initarg :stream
            :initform nil
            :type (or stream null)
            :reader read-stream)))


(defun close-silence-errors (stream) ; in case if closing already close streams produces error
  (handler-case (close stream)
    (error (e) (declare (ignore e)))))


(defgeneric close-inner-stream (range)
  (:method ((range file-range-mixin))
    (when-let ((stream (read-stream range)))
      (close-silence-errors stream)
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


(defmethod initialize-instance :after ((range file-range-mixin)
                                       &rest all)
  (declare (ignore all))
  (unless (null (read-stream range))
    (trivial-garbage:finalize
     range
     (curry #'close-silence-errors (read-stream range)))))


(defun close-stream (range)
  (unless (~> range read-stream null)
     (~> range read-stream close-silence-errors)
    (setf (slot-value range '%stream) nil)))
