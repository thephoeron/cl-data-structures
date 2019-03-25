(in-package #:cl-data-structures.utils.clustering.k-means)


(-> k-means (vector positive-fixnum non-negative-single-float
                    &key
                    (:silhouette-sample-size (or null positive-fixnum))
                    (:silhouette-sample-count (or null positive-fixnum))
                    (:value-key (or null function)))
    cl-ds.utils.cluster:clustering-result)
(defun k-means (data medoids-count distortion-epsilon
                &rest all
                &key silhouette-sample-size silhouette-sample-count value-key)
  (declare (ignore value-key silhouette-sample-size silhouette-sample-count))
  (iterate
    (with state = (make-state data medoids-count
                              distortion-epsilon all))
    (assign-data-points-to-medoids state)
    (select-new-medoids state)
    (for distortion = (distortion state))
    (for prev-distortion
         previous distortion
         initially nil)
    (while (or (null prev-distortion)
               (< (abs (- distortion prev-distortion))
                  distortion-epsilon)))
    (finally (return (obtain-result state)))))
