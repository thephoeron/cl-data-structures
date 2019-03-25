(in-package #:cl-data-structures.clustering)


(defclass clara-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass clara-variable-number-of-medoids (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric clara (range number-of-medoids sample-size
                   sample-count metric-fn
                   &key
                     key select-medoids-attempts-count
                     attempts split merge)
  (:generic-function-class clara-function)
  (:method (range number-of-medoids sample-size
            sample-count metric-fn &key
              (key #'identity) (select-medoids-attempts-count 50)
              (attempts 0) split merge)
    (cl-ds.alg.meta:apply-aggregation-function
     range #'clara
     :key key
     :number-of-medoids number-of-medoids
     :sample-size sample-size
     :sample-count sample-count
     :metric-fn metric-fn
     :select-medoids-attempts-count select-medoids-attempts-count
     :attempts attempts
     :split split
     :merge merge)))


(defgeneric clara-variable-number-of-medoids (range from to sample-size
                                              sample-count metric-fn
                                              &key
                                                key select-medoids-attempts-count
                                                attempts split merge)
  (:generic-function-class clara-variable-number-of-medoids)
  (:method (range from to sample-size
            sample-count metric-fn
            &key (key #'identity) (select-medoids-attempts-count 50)
              (attempts 0) split merge)
    (cl-ds.alg.meta:apply-aggregation-function
     range #'clara-variable-number-of-medoids
     :key key
     :from from
     :to to
     :sample-size sample-size
     :sample-count sample-count
     :metric-fn metric-fn
     :select-medoids-attempts-count select-medoids-attempts-count
     :attempts attempts
     :split split
     :merge merge)))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function clara-variable-number-of-medoids)
     &rest all
     &key
       from to sample-size sample-count metric-fn
       key select-medoids-attempts-count attempts split merge
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (cl-ds.utils.cluster.clara/pam:clara-variable-number-of-medoids
           vector
           metric-fn sample-size sample-count from to
           :key key
           :select-medoids-attempts-count select-medoids-attempts-count
           :attempts attempts :split split :merge merge))))
