(in-package #:cl-data-structures.math)


(defclass moments-function (cl-ds.alg.meta:aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric moments (range from count about &key key)
  (:generic-function-class moments-function)
  (:method (range from count about &key (key #'identity))
    (cl-ds.alg.meta:apply-aggregation-function range
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
   (%count :initform 0
           :accessor access-count)
   (%lambdas :initarg :lambdas
             :accessor read-lambdas)))

(in-package #:cl-data-structures.math)


(defclass hodges-lehman-estimator-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric hodges-lehman-estimator (range &key key)
  (:generic-function-class hodges-lehman-estimator-function)
  (:method (range &key (key #'identity))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'hodges-lehman-estimator
                                               :key key)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function hodges-lehman-estimator-function)
     &rest all
     &key key
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :element-type 'real))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (bind ((length (length vector))
                 (median-length (/ (* length (1+ length)) 2))
                 (middle (truncate median-length 2))
                 (median-buffer (make-array median-length
                                            :element-type 'single-float))
                 (position 0))
            (iterate
              (for i from 0 below length)
              (iterate
                (for j from i below length)
                (setf (aref median-buffer (finc position))
                      (coerce (/ (+ (aref vector i)
                                    (aref vector j))
                                 2)
                              'single-float))))
            (setf median-buffer (sort median-buffer #'<))
            (if (oddp median-length)
                (aref median-buffer middle)
                (/ (+ (aref median-buffer middle)
                      (aref median-buffer (1- middle)))
                   2))))))

(defmethod cl-ds.alg.meta:make-state ((function moments-function)
                                      &rest all
                                      &key count about from)
  (declare (ignore all))
  (check-type count positive-fixnum)
  (check-type from positive-fixnum)
  (check-type about real)
  (bind ((lambdas (make-array count))
         (result (make-array count :element-type 'number)))
    (iterate
      (for i from from)
      (for index from 0 below count)
      (setf (aref lambdas index) (let ((power i))
                                   (lambda (value)
                                     (expt (- value about) power)))))
    (make 'moments-state :moments result
                         :lambdas lambdas
                         :start from)))


(defmethod cl-ds.alg.meta:aggregate ((function moments-function)
                                     state
                                     element)
  (check-type state moments-state)
  (bind (((:slots %lambdas %moments %count) state))
    (incf %count)
    (iterate
      (for i from 0 below (length %lambdas))
      (incf (aref %moments i) (funcall (aref %lambdas i) element)))))


(defmethod cl-ds.alg.meta:state-result ((function moments-function)
                                        state)
  (check-type state moments-state)
  (let ((moments (read-last-moments state))
        (count (access-count state)))
    (map-into moments (rcurry #'/ count) moments)
    (make-instance 'cl-ds.adapters:offset-vector-range
                   :vector moments
                   :offset (read-start state))))
