(in-package #:cl-data-structures.math)


(defclass moments-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric moments (range upto about &key key)
  (:generic-function-class moments-function)
  (:method (range upto about &key (key #'identity))
    (cl-ds.alg:apply-aggregation-function range
                                          #'distribution-moments
                                          :upto upto
                                          :around about
                                          :key key)))


(defclass moments-state ()
  ((%moments :initarg :last-moments
             :reader read-last-moments)
   (%key :initarg :key :reader read-key)
   (%lambdas :initarg :lambdas
             :accessor read-lambdas)))


(defmethod cl-ds.alg:make-state ((function moments-function)
                                 &rest all
                                 &key upto about key)
  (declare (ignore all))
  (bind ((lambdas (make-array upto))
         (result (make-array upto :element-type 'number)))
    (iterate
      (for i from 1)
      (for index from 0 below upto)
      (setf (aref lambdas index) (let ((i i))
                                   (lambda (value)
                                     (expt (- value about) i)))))
    (make 'moments-state :moment result
                         :lambdas lambdas
                         :key key)))


(defmethod cl-ds.alg:aggregate ((function moments-function)
                                state
                                element)
  (check-type state distribution-moment-state)
  (bind (((:slots %lambdas %key %moments) state)
         (element (funcall %key element)))
    (iterate
      (for i from 0 below (length %lambdas))
      (incf (aref %moments i) (funcall (aref %lambdas 0) element)))))


(defmethod cl-ds.alg:state-result ((function moments-function)
                                   state)
  (check-type state moments-state)
  (read-last-moments state))
