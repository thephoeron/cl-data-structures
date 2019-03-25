(in-package #:cl-data-structures.utils.clustering.k-means)


(defun select-initial-medoids (state)
  (let ((medoids-count (read-medoids-count state))
        (medoids (access-medoids state))
        (data (read-data state)))
    (setf medoids (adjust-array medoids medoids-count
                                :fill-pointer medoids-count))
    (cl-ds.utils:draw-random-vector data medoids-count)
    (setf (access-medoids state) medoids)
    state))


(defun assign-data-points-to-medoids (state)
  cl-ds.utils:todo)


(defun distortion (state)
  (cl-ds.utils:with-slots-for (state k-means-algorithm-state)
    (iterate outer
      (for cluster in-vector %clusters)
      (for medoid in-vector %medoids)
      (iterate
        (declare (type fixnum size i))
        (with size = (length cluster))
        (for i from 0 below size)
        (for c = (aref cluster i))
        (iterate
          (declare (type fixnum size i))
          (with size = (length c))
          (for i from 0 below size)
          (for error = (- (the single-float (aref c i))
                          (the single-float (aref medoid i))))
          (in outer (sum (expt error 2))))))))


(defun obtain-result (state)
  cl-ds.utils:todo)


(defun make-state (data medoids-count distortion-epsilon)
  (make 'k-means-algorithm-state
        :data data
        :medoids-count medoids-count
        :distortion-epsilon distortion-epsilon))
