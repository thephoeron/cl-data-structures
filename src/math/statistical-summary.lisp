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


(defmethod cl-ds.alg.meta:make-state ((function statistical-summary-function)
                                      &rest all &key key moments average)
  (declare (ignore all key))
  (list* average moments))


(defmethod cl-ds.alg.meta:aggregation-finished-p ((function statistical-summary-function)
                                                  state)
  t)


(defmethod cl-ds.alg.meta:multi-aggregation-stages ((fn statistical-summary-function)
                                                    &rest all
                                                    &key key
                                                    &allow-other-keys)
  (declare (ignore all))
  (list (list* :average (lambda (range &rest all)
                          (declare (ignore all))
                          (average range :key key)))
        (list* :moments (lambda (range &key average key)
                          (moments range
                                   2 3
                                   average
                                   :key key)))))


(defmethod cl-ds.alg.meta:state-result ((fn statistical-summary-function)
                                        state)
  (bind (((average . moments) state)
         (variance (cl-ds:at moments 2))
         (sd (sqrt variance))
         (skewness (/ (cl-ds:at moments 3) (expt sd 3)))
         (kurtosis (/ (cl-ds:at moments 4) (expt sd 4))))
    (cl-ds.alg:make-hash-table-range
     (dict :average average
           :variance variance
           :skewness skewness
           :kurtosis kurtosis))))
