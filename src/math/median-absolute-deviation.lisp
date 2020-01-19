(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    median-absolute-deviation median-absolute-deviation-function

    (:range &key key)
    (:range &key (key #'identity))

    (%data)

    ((setf %data (vect)))

    ((element)
     (vector-push-extend element %data))

    ((let ((vector %data))
       (declare (type (cl-ds.utils:extendable-vector t) vector))
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
