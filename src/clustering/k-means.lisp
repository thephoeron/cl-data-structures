(in-package #:cl-data-structures.clustering)


(defclass k-means-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric k-means (range number-of-medoids distortion-epsilon
                     &rest all
                     &key silhouette-sample-size iterations
                       silhouette-sample-count value-key)
  (:generic-function-class k-means-function)
  (:method (range number-of-medoids distortion-epsilon
            &rest all
            &key silhouette-sample-size iterations
              silhouette-sample-count value-key)
    (declare (ignore silhouette-sample-size iterations
                     silhouette-sample-count value-key))
    (apply #'cl-ds.alg.meta:apply-range-function
           range #'k-means
           :number-of-medoids number-of-medoids
           :distortion-epsilon distortion-epsilon
           all)))


(defmethod cl-ds.alg.meta:multi-aggregation-function
    ((function k-means-function)
     &rest all
     &key number-of-medoids distortion-epsilon)
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (apply #'cl-ds.utils.cluster.k-means:k-means
                 vector number-of-medoids distortion-epsilon
                 (cddr all)))))
