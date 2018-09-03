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


(defmethod cl-ds:peek-front ((range constrainted-range))
  (bind ((current (access-current range))
         ((:labels value (element))
          (cond ((null element) nil)
                ((atom element) element)
                ((listp element)
                 (iterate
                   (for elt in element)
                   (for result = (value elt))
                   (until result)
                   (finally (return result))))))
         (result (value current)))
    (if (null result)
        (~> range cl-ds:clone cl-ds:consume-front)
        (values result t))))


(defmethod cl-ds:consume-front ((range constrainted-range))
  (bind (((:accessor (current access-current)) range)
         ((:labels value ())
          (cond ((null current) nil)
                ((atom current) (shiftf current nil))
                ((listp current)
                 (iterate
                   (for elt = (first current))
                   (setf current (rest current))
                   (cond ((listp elt) (setf current (append elt current)))
                         ((atom elt) (leave elt)))
                   (until (null current))))))
         (result (value)))
    (if (null result)
        (let ((val (~> range access-monad funcall)))
          (if (eql val :end)
              (values nil nil)
              (progn (setf current val)
                     (cl-ds:consume-front range))))
        (values result t))))
