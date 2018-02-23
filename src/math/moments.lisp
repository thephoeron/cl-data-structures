(in-package #:cl-data-structures.math)


(defclass moments-function (cl-ds.alg:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric moments (range from count about &key key)
  (:generic-function-class moments-function)
  (:method (range from count about &key (key #'identity))
    (cl-ds.alg:apply-aggregation-function range
                                          #'moments
                                          :count count
                                          :from from
                                          :about about
                                          :key key)))


(defclass moments-state ()
  ((%moments :initarg :moments
             :reader read-last-moments)
   (%start :initarg :start
           :reader read-start)
   (%key :initarg :key :reader read-key)
   (%lambdas :initarg :lambdas
             :accessor read-lambdas)))


(defmethod cl-ds.alg:make-state ((function moments-function)
                                 &rest all
                                 &key count about key from)
  (declare (ignore all)
           (optimize (debug 3)))
  (assert (> from 0))
  (bind ((lambdas (make-array count))
         (result (make-array count :element-type 'number)))
    (iterate
      (for i from from)
      (for index from 0 below count)
      (setf (aref lambdas index) (let ((i i))
                                   (lambda (value)
                                     (expt (- value about) i)))))
    (make 'moments-state :moments result
                         :lambdas lambdas
                         :key key
                         :start from)))


(defmethod cl-ds.alg:aggregate ((function moments-function)
                                state
                                element)
  (check-type state moments-state)
  (bind (((:slots %lambdas %key %moments) state)
         (element (funcall %key element)))
    (iterate
      (for i from 0 below (length %lambdas))
      (incf (aref %moments i) (funcall (aref %lambdas i) element)))))


(defmethod cl-ds.alg:state-result ((function moments-function)
                                   state)
  (check-type state moments-state)
  (make-instance 'cl-ds.adapters:offset-vector-range
                 :vector (read-last-moments state)
                 :offset (read-start state)))
