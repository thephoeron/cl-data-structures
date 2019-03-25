(in-package #:cl-data-structures.utils.clustering.k-means)


(defclass k-means-algorithm-state ()
  ((%data :initarg :data
          :type vector
          :reader read-data)
   (%value-key :initarg :value-key
               :type function
               :reader read-value-key)
   (%clusters :initarg :clusters
              :type vector
              :reader read-clusters)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :reader read-distortion-epsilon)
   (%medoids :initarg :medoids
             :type vector
             :accessor access-medoids)
   (%silhouette-sample-size :initarg :silhouette-sample-size
                            :type fixnum
                            :initform 500
                            :reader silhouette-sample-size)
   (%silhouette-sample-count :initarg :silhouette-sample-count
                             :initform 15
                             :type fixnum
                             :reader silhouette-sample-count)
   (%medoids-count :initarg :medoids-count
                   :type non-negative-fixnum
                   :reader read-medoids-count))
  (:default-initargs
   :clusters (vect)
   :medoids (vect)
   :data (vect)))


(cl-ds.utils:define-list-of-slots k-means-algorithm-state
  (%data read-data)
  (%clusters read-clusters)
  (%value-key read-value-key)
  (%distortion-epsilon read-distortion-epsilon)
  (%silhouette-sample-size silhouette-sample-size)
  (%silhouette-sample-count silhouette-sample-count)
  (%medoids access-medoids)
  (%medoids-count read-medoids-count))


(defmethod cl-ds.utils:cloning-information
    :append ((obj k-means-algorithm-state))
  '((:data read-data)
    (:clusters access-clusters)
    (:medoids access-medoids)
    (:value-key read-value-key)
    (:silhouette-sample-size silhouette-sample-size)
    (:silhouette-sample-count silhouette-sample-count)
    (:distortion-epsilon read-distortion-epsilon)
    (:medoids-count read-medoids-count)))
