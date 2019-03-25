(in-package #:cl-data-structures.utils.clustering.k-means)


(defclass k-means-algorithm-state ()
  ((%data :initarg :data
          :type vector
          :reader read-data)
   (%clusters :initarg :clusters
              :type vector
              :reader read-clusters)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :reader read-distortion-epsilon)
   (%medoids :initarg :medoids
             :type vector
             :accessor access-medoids)
   (%medoids-count :initarg :medoids-count
                   :type non-negative-fixnum
                   :reader read-medoids-count))
  (:default-initargs
   :clusters (vect)
   :medoids (vect)
   :data (vect)))


(defmethod cl-ds.utils:cloning-information
    :append ((obj k-means-algorithm-state))
  '((:data read-data)
    (:clusters access-clusters)
    (:medoids access-medoids)
    (:distorction-epsilon read-distortion-epsilon)
    (:medoids-count read-medoids-count)))
