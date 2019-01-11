(in-package #:cl-data-structures.file-system)


(defclass file-range-mixin ()
  ((%stream :initarg :stream
            :initform (list nil)
            :type list)))


(defun read-stream (object)
  (check-type object file-range-mixin)
  (car (slot-value object '%stream)))


(defun close-silence-errors (stream) ; in case if closing already close streams produces error
  (handler-case (close stream)
    (error (e) (declare (ignore e)))))


(defgeneric close-inner-stream (range)
  (:method ((range cl-ds:fundamental-forward-range))
    (cl-ds:forward-call range #'close-inner-stream nil))
  (:method ((range file-range-mixin))
    (when-let ((stream (read-stream range)))
      (close-silence-errors stream)
      (setf (car (slot-value range '%stream)) nil))
    range))


(defmacro with-file-ranges (bindings &body body)
  (let ((extra-vars (mapcar (lambda (x) (gensym)) bindings))
        (prime-vars (mapcar #'first bindings)))
    (with-gensyms (!tmp)
      `(let (,@extra-vars)
         (declare (ignorable ,@extra-vars))
         (unwind-protect
              (progn
                ,@(mapcar (lambda (extra x)
                            `(setf ,extra
                                   (let ((,!tmp ,(second x)))
                                     (check-type ,!tmp file-range-mixin)
                                     ,!tmp)))
                          extra-vars
                          bindings)
                (let ,(mapcar #'list prime-vars extra-vars)
                  ,@body))
           (progn
             ,@(mapcar (lambda (x) `(unless (null ,x)
                                 (close-inner-stream ,x)))
                       extra-vars)))))))


(defun enclose-finalizer (stream-cons)
  (lambda ()
    (unless (null (car stream-cons))
      (close-silence-errors (car stream-cons)))))


(defmethod initialize-instance :after ((range file-range-mixin)
                                       &rest all)
  (declare (ignore all))
  (trivial-garbage:finalize range
                            (enclose-finalizer (slot-value range '%stream))))


(defun close-stream (range)
  (unless (~> range read-stream null)
     (~> range read-stream close-silence-errors)
     (setf (car (slot-value range '%stream)) nil)))
