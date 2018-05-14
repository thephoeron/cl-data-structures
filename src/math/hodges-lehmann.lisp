(in-package #:cl-data-structures.math)


(defclass hodges-lehmann-estimator-function
    (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric hodges-lehmann-estimator (range &key key parallel)
  (:generic-function-class hodges-lehmann-estimator-function)
  (:method (range &key (key #'identity) (parallel nil))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'hodges-lehmann-estimator
                                               :parallel parallel
                                               :key key)))


(defun calculate-hodges-lehmann-estimator
    (parallel &key vector &allow-other-keys)
  (declare (type (vector real) vector))
  (bind ((length (length vector))
         ((:dflet index (i j))
          (+ (- (* length i)
                (/ (* i (1+ i)) 2)
                1 i)
             j))
         (median-length (1+ (index (1- length) (1- length))))
         (middle (truncate median-length 2))
         (median-buffer (make-array median-length :element-type 'real))
         (indexes (~> length iota (coerce '(vector fixnum))))
         ((:dflet average-of-pairs (i))
          (iterate
            (for j from (1+ i) below length)
            (for index = (index i j))
            (setf (aref median-buffer index)
                  (/ (+ (aref vector i)
                        (aref vector j))
                     2)))))
    (funcall (if parallel #'lparallel:pmap #'map)
             nil #'average-of-pairs indexes)
    (setf median-buffer (funcall (if parallel #'lparallel:psort #'sort)
                                 median-buffer #'<))
    (if (oddp median-length)
        (aref median-buffer middle)
        (/ (+ (aref median-buffer middle)
              (aref median-buffer (1- middle)))
           2))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function hodges-lehmann-estimator-function)
     &rest all
     &key key
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key
                                     :element-type 'real
                                     :force-copy nil))
        (curry #'calculate-hodges-lehmann-estimator parallel)))
