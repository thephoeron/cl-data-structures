(cl:in-package #:cl-data-structures.math)


(defclass linear-regression-fit (c2mop:funcallable-standard-object)
  ((%beta1 :initarg :beta1
           :accessor beta1)
   (%beta0 :initarg :beta0
           :accessor beta0))
  (:metaclass c2mop:funcallable-standard-class))


(defmethod initialize-instance :after ((obj linear-regression-fit) &rest rest)
  (declare (ignore rest))
  (c2mop:set-funcallable-instance-function obj (lambda (x)
                                                 (+ (* (beta1 obj) x)
                                                    (beta0 obj)))))


(cl-ds.alg.meta:define-aggregation-function
    simple-linear-regression
    simple-linear-regression-function

    (:range x-key average-x y-key average-y &key key)
    (:range x-key average-x y-key average-y &key (key #'identity))

    (%x-key %y-key %average-x %average-y %yy %xy %xx)

    ((setf %x-key x-key
           %y-key y-key
           %average-x average-x
           %average-y average-y
           %yy 0
           %xy 0
           %xx 0))

    ((element)
     (let ((x (funcall %x-key element))
           (y (funcall %y-key element)))
       (incf %yy (expt (- y %average-y) 2))
       (incf %xy (* (- y %average-y) (- x %average-x)))
       (incf %xx (expt (- x %average-x) 2))))

    ((let* ((beta1 (/ %xy %xx))
            (beta0 (- %average-y (* beta1 %average-x))))
       (make 'linear-regression-fit
             :beta0 beta0
             :beta1 beta1))))
