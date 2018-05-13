(in-package #:cl-data-structures.math)


(defclass hodges-lehman-estimator-function
    (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass approximated-hodges-lehman-estimator-function
    (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric approximated-hodges-lehman-estimator
    (range sample-size samples-count &key key)
  (:generic-function-class approximated-hodges-lehman-estimator-function)
  (:method (range sample-size samples-count &key (key #'identity))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'approximated-hodges-lehman-estimator
                                               :key key
                                               :sample-size sample-size
                                               :samples-count samples-count)))


(defgeneric hodges-lehman-estimator (range &key key)
  (:generic-function-class hodges-lehman-estimator-function)
  (:method (range &key (key #'identity))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'hodges-lehman-estimator
                                               :key key)))


(defun calculate-hodges-lehman-estimator
    (&key vector &allow-other-keys)
  (declare (type (vector real) vector))
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
           2))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function hodges-lehman-estimator-function)
     &rest all
     &key key
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :element-type 'real))

        #'calculate-hodges-lehman-estimator))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function approximated-hodges-lehman-estimator-function)
     &rest all
     &key key samples-count sample-size
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :element-type 'real))

        (bootstrap (&key vector &allow-other-keys)
                   vector
                   data
                   (calculate-hodges-lehman-estimator :vector data)
                   samples-count
                   sample-size)))
