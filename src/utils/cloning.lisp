(cl:in-package #:cl-data-structures.utils)


(defgeneric cloning-information (object)
  (:method-combination append :most-specific-last))


(defun cloning-list (instance)
  (iterate
    (for (initarg reader) in (cloning-information instance))
    (collect initarg)
    (collect (funcall reader instance))))


(defun clone (instance)
  (apply #'make-instance
         (class-of instance)
         (cloning-list instance)))


(defmacro quasi-clone* (instance &body arguments)
  (once-only (instance)
    `(apply #'make-instance (class-of ,instance)
            ,@arguments
            (cloning-list ,instance))))


(defun quasi-clone (instance initarg initval)
  (quasi-clone* instance initarg initval))
