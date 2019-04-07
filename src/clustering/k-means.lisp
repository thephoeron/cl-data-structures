(in-package #:cl-data-structures.clustering)


(defclass k-means-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric k-means (range number-of-medoids distortion-epsilon
                     &rest all
                     &key key silhouette-sample-size iterations
                       silhouette-sample-count)
  (:generic-function-class k-means-function)
  (:method (range number-of-medoids distortion-epsilon
            &rest all
            &key (key #'identity)
              (silhouette-sample-size 500)
              (iterations nil)
              (silhouette-sample-count 15))
    (declare (ignore silhouette-sample-size iterations
                     silhouette-sample-count key))
    (apply #'cl-ds.alg.meta:apply-range-function
           range #'k-means
           :number-of-medoids number-of-medoids
           :distortion-epsilon distortion-epsilon
           all)))


(defmethod cl-ds.alg.meta:multi-aggregation-function
    ((function k-means-function)
     &rest all
     &key number-of-medoids distortion-epsilon
       silhouette-sample-size iterations
       silhouette-sample-count)
  (check-type number-of-medoids integer)
  (check-type distortion-epsilon single-float)
  (check-type silhouette-sample-size integer)
  (check-type silhouette-sample-count integer)
  (check-type iterations (or null integer))
  (cl-ds:check-argument-bounds distortion-epsilon
                               (< 0.0 distortion-epsilon))
  (cl-ds:check-argument-bounds silhouette-sample-size
                               (< 0 silhouette-sample-size))
  (cl-ds:check-argument-bounds silhouette-sample-count
                               (< 0 silhouette-sample-count))
  (cl-ds:check-argument-bounds number-of-medoids
                               (< 0 number-of-medoids))
  (unless (null iterations)
    (cl-ds:check-argument-bounds iterations
                                 (< 0 iterations)))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (apply #'cl-ds.utils.cluster.k-means:k-means
                 vector number-of-medoids distortion-epsilon
                 (cddr all)))))
