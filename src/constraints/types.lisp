(in-package #:cl-data-structures.constraints)


(defclass input ()
  ((%range :initarg :range
           :reader read-range
           :writer write-range)
   (%depth :initarg :depth
           :reader read-depth)))


(defclass constrainted-range ()
  ((%constructor :initarg :constructor
                 :reader read-constructor)
   (%monad :initarg :monad
           :accessor access-monad)
   (%current :initarg :current
             :accessor access-current)
   (%original-current :initarg :original-current
                      :reader read-original-current)
   (%inputs :initarg :inputs
            :reader read-inputs)))


(defun same-depth (input depth)
  (eql depth (read-depth input)))


(defun value (something)
  (cond ((typep something 'input)
         (nth-value 0 (cl-ds:peek-front (read-range something))))
        (t something)))


(defun reached-end (something)
  (cond ((typep something 'input)
         (not (nth-value 1 (~> something read-range cl-ds:peek-front))))
        (t something)))


(defun make-input (deps depth function)
  (declare (ignore depth))
  (setf deps (remove-if-not (rcurry #'typep 'input) deps))
  (iterate
    (for dep in deps)
    (for d = (read-depth dep))
    (minimize d into minimal-depth)
    (finally
     (return (make-instance 'input
                            :depth minimal-depth
                            :range (apply #'cl-ds.alg:cartesian function
                                          (mapcar #'read-range deps)))))))


(defun monadic-value (value rejected)
  (list value rejected))


(defun accepted-monadic-value (values)
  (list values nil))

(defun rejected-monadic-value (rejected)
  (list nil rejected))
