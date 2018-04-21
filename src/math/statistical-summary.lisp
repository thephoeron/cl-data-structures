(in-package #:cl-data-structures.math)


(defclass statistical-summary-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric statistical-summary (range &key key)
  (:generic-function-class statistical-summary-function)
  (:method (range &key (key #'identity))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'statistical-summary
                                               :key key)))


(flet ((final (range &key average moments &allow-other-keys)
         (declare (ignore range))
         (bind ((variance (cl-ds:at moments 2))
                (sd (sqrt variance))
                (skewness (/ (cl-ds:at moments 3) (expt sd 3)))
                (kurtosis (/ (cl-ds:at moments 4) (expt sd 4))))
           (cl-ds.alg:make-hash-table-range
            (dict :average average
                  :variance variance
                  :skewness skewness
                  :kurtosis kurtosis)))))
  (defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn statistical-summary-function)
                                                      &rest all
                                                      &key key
                                                      &allow-other-keys)
    (declare (ignore all))
    (list (cl-ds.alg.meta:stage :average (range &rest all)
            (declare (ignore all))
            (average range :key key))

          (cl-ds.alg.meta:stage :moments (range &key average key)
            (moments range 2 3 average :key key))

          #'final)))
