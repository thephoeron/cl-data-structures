(in-package #:cl-data-structures.file-system)


(defclass file-range-mixin ()
  ((%stream :initarg :stream
            :type (or stream null)
            :reader read-stream)))


(defgeneric close-inner-stream (range)
  (:method ((range file-range-mixin))
    (when-let ((stream (read-stream range)))
      (close stream))))


(defmacro with-file-ranges (bindings &body body)
  `(let ,(mapcar (lambda (x) (list (first x) nil)) bindings)
     (unwind-protect
          (progn
            ,@(mapcar (lambda (x) `(setf ,(first x) ,(second x))) bindings)
            ,@body)
       (progn
         ,@(mapcar (lambda (x) `(unless (null ,(first x))
                             (close-inner-stream ,(first x))))
                   bindings)))))
