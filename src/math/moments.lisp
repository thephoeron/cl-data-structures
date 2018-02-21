(in-package #:cl-data-structures.math)


(defclass moments-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric moments (range from count about &key key)
  (:generic-function-class moments-function)
  (:method (range count about from &key (key #'identity))
    (cl-ds.alg:apply-aggregation-function range
                                          #'distribution-moments
                                          :upto count
                                          :from from
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
                                 &key count about key from)
  (declare (ignore all))
  (assert (> from 0))
  (bind ((lambdas (make-array count))
         (result (make-array count :element-type 'number)))
    (iterate
      (for i from from)
      (for index from 0 below count)
      (setf (aref lambdas index) (let ((i i))
                                   (lambda (value)
                                     (expt (- value about) i)))))
    (make 'moments-state :moment result
                         :lambdas lambdas
                         :key key)))


(defmethod cl-ds.alg:aggregate ((function moments-function)
                                state
                                element)
  (check-type state moments-state)
  (bind (((:slots %lambdas %key %moments) state)
         (element (funcall %key element)))
    (iterate
      (for i from 0 below (length %lambdas))
      (incf (aref %moments i) (funcall (aref %lambdas 0) element)))))


(defmethod cl-ds.alg:state-result ((function moments-function)
                                   state)
  (check-type state moments-state)
  (read-last-moments state))
