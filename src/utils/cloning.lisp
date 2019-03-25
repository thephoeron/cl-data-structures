(in-package #:cl-data-structures.utils)


(defgeneric cloning-information (object)
  (:method-combination append :most-specific-last))


(defun clone (instance)
  (apply #'make-instance
         (class-of instance)
         (iterate
           (for (initarg reader) in (cloning-information instance))
           (collect initarg)
           (collect (funcall reader instance)))))


(defun quasi-clone (instance initarg initval)
  (apply #'make-instance
         (class-of instance)
         initarg initval
         (iterate
           (for (initarg reader) in (cloning-information instance))
           (collect initarg)
           (collect (funcall reader instance)))))


(defmacro quasi-clone* (instance &body arguments)
  (once-only (instance)
    `(apply #'make-instance (class-of ,instance)
            ,@arguments
            (cloning-information ,instance))))
