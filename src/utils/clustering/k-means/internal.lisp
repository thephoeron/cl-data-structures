(in-package #:cl-data-structures.utils.clustering.k-means)


(defun select-initial-medoids (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (setf %medoids (cl-ds.utils:draw-random-vector %data %medoids-count))
    (cl-ds.utils:transform %value-key %medoids))
  state)


(defun assign-data-points-to-medoids (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (iterate
      (for cluster in-vector %clusters)
      (setf (fill-pointer cluster) 0))
    (lparallel:pmap nil
                    (lambda (data-point
                             &aux (data (funcall %value-key data-point)))
                      (iterate
                        (for i from 0 below (length %medoids))
                        (for medoid in-vector %medoids)
                        (for distance = (cl-ds.utils.metric:euclid-metric medoid
                                                                          data))
                        (finding i minimizing distance)
                        (finally (vector-push-extend data-point
                                                     (aref %clusters i)))))
                    %data))
  state)


(defun distortion (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (iterate outer
      (for cluster in-vector %clusters)
      (for medoid in-vector %medoids)
      (iterate
        (declare (type fixnum size i))
        (with size = (length cluster))
        (for i from 0 below size)
        (for c = (funcall %value-key (aref cluster i)))
        (iterate
          (declare (type fixnum size i))
          (with size = (length c))
          (for i from 0 below size)
          (for error = (- (the single-float (aref c i))
                          (the single-float (aref medoid i))))
          (in outer (sum (expt error 2))))))))


(defun obtain-result (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (make 'cl-ds.utils.cluster:clustering-result
          :cluster-contents %clusters
          :distance-function #'cl-ds.utils.metric:euclid-metric
          :silhouette-sample-size %silhouette-sample-size
          :key-function %value-key
          :silhouette-sample-count %silhouette-sample-count)))


(defun make-state (data medoids-count distortion-epsilon all)
  (~> (apply #'make 'k-means-algorithm-state
             :data data
             :cluster-contents (~> (make-array medoids-count)
                                   (map-into #'vect))
             :medoids (make-array medoids-count)
             :medoids-count medoids-count
             :distortion-epsilon distortion-epsilon
             all)
      select-initial-medoids))


(defun select-new-medoids (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (iterate
      (for i from 0)
      (for cluster in-vector %clusters)
      (for medoid in-vector %medoids)
      (for new-medoid = (make-array (length medoid)
                                    :element-type 'single-float
                                    :initial-element 0.0))
      (iterate
        (for c in-vector cluster)
        (map-into new-medoid #'+ new-medoid c))
      (cl-ds.utils:transform (rcurry #'/ (length cluster)) new-medoid)
      (setf (aref %medoids i) new-medoid)))
  state)
