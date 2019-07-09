(in-package #:cl-data-structures.math)


(defclass median-absolute-deviation-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric median-absolute-deviation (range &key key)
  (:generic-function-class median-absolute-deviation-function)
  (:method (range &key (key #'identity))
    (cl-ds.alg.meta:apply-range-function range
                                         #'median-absolute-deviation
                                         :key key)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function median-absolute-deviation-function)
     &rest all
     &key key
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :element-type 'real))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (setf vector (sort vector #'<))
          (bind ((length (length vector))
                 (middle (truncate length 2))
                 (median (if (oddp length)
                             (aref vector middle)
                             (coerce (/ (+ (aref vector middle)
                                           (aref vector (1- middle)))
                                        2)
                                     'single-float))))
            (declare (type real median))
            (map-into vector
                      (lambda (x)
                        (declare (type real x))
                        (abs (- x median)))
                      vector)
            (setf vector (sort vector #'<))
            (if (oddp length)
                (aref vector middle)
                (/ (+ (aref vector middle)
                      (aref vector (1- middle)))
                   2))))))
