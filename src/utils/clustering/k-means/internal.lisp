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
    (let ((locks (~> (make-array (length %clusters))
                     (cl-ds.utils:transform #'bt:make-lock))))
      (lparallel:pmap
       nil
       (lambda (data-point
                &aux (data (funcall %value-key data-point)))
         (iterate
           (for i from 0 below (length %medoids))
           (for medoid in-vector %medoids)
           (for distance =
                (cl-ds.utils.metric:euclid-metric medoid data))
           (finding i minimizing distance)
           (finally (bt:with-lock-held ((aref locks i))
                      (vector-push-extend data-point (aref %clusters i))))))
       %data)))
  state)


(defun distortion (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (~>> (lparallel:pmap
          '(vector single-float)
          (lambda (cluster medoid)
            (iterate outer
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
                (in outer (sum (expt error 2))))))
          %clusters
          %medoids)
         (reduce #'+))))


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
    (setf %medoids
          (lparallel:pmap
           'vector
           (lambda (cluster medoid)
             (iterate
               (with new-medoid = (make-array (length medoid)
                                              :element-type 'single-float
                                              :initial-element 0.0))
               (for c in-vector cluster)
               (map-into new-medoid #'+ new-medoid c)
               (finally
                (return (cl-ds.utils:transform (rcurry #'/ (length cluster))
                                               new-medoid)))))
           %clusters
           %medoids)))
  state)
