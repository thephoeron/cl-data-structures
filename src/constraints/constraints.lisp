(in-package #:cl-data-structures.constraints)


(defun constraints (body)
  (let* ((*all-inputs* (vect))
         (constructor body)
         (monad (funcall constructor)))
    (make 'constrainted-range
          :constructor constructor
          :monad monad
          :inputs *all-inputs*)))


(defmethod cl-ds:reset! ((range constrainted-range))
  (map nil (compose #'cl-ds:reset! #'read-range) (read-inputs range))
  (setf (access-current range) (read-original-current range))
  range)


(defmethod cl-ds:clone ((range constrainted-range))
  (bind ((inputs (read-inputs range))
         (*all-inputs* (vect))
         (constructor (read-constructor range))
         (monad (funcall constructor)))
    (map nil (lambda (new-input old-input)
               (~> old-input read-range cl-ds:clone
                   (write-range new-input)))
         *all-inputs* inputs)
    (make 'constraints
          :constructor constructor
          :current (access-current range)
          :original-current (access-current range)
          :monad monad
          :inputs *all-inputs*)))


(defmethod cl-ds:consume-front ((range constrainted-range))
  )


(defmethod cl-ds:peek-front ((range constrainted-range))
  )
