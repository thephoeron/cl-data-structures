(cl:in-package #:cl-data-structures.math)


(defclass gather-z-score-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric gather-z-score (range &key key biased)
  (:generic-function-class gather-z-score-function)
  (:method (range &key key (biased t))
    (cl-ds.alg.meta:apply-range-function range
                                         #'gather-z-score
                                         :key key
                                         :biased biased)))


(defclass z-score ()
  ((%standard-deviation :initarg :standard-deviation
                        :reader read-standard-deviation)
   (%average :initarg :average
             :reader read-average)))


(defun make-z-score (average standard-deviation)
  (check-type average real)
  (check-type standard-deviation real)
  (make 'z-score :standard-deviation standard-deviation
                 :average average))


(defun z-score (z-score element)
  (check-type z-score z-score)
  (check-type element real)
  (/ (- element (read-average z-score))
     (read-standard-deviation z-score)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((fn gather-z-score-function)
     &rest all &key key &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :average (range &rest all)
          (declare (ignore all))
          (average range :key key))
        (cl-ds.alg.meta:stage :variance (range &key average &allow-other-keys)
          (moments range 2 1 average :key key))
        (lambda (&key average variance)
          (make-z-score average (sqrt (cl-ds:at variance 2))))))
