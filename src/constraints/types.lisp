(in-package #:cl-data-structures.constraints)


(defclass input ()
  ((%range :initarg :range
           :reader read-range)
   (%depth :initarg :depth
           :reader read-depth)))


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


(defun monadic-value (value &rest rejected)
  (list value
        (iterate
          (with shallow = nil)
          (for rej in rejected)
          (for depth = (read-depth rej))
          (minimize depth into min)
          (when (eql min depth)
            (setf shallow rej))
          (finally (return shallow)))))


(defun accepted-monadic-value (value &rest values)
  (list (cons value values)
        nil))
