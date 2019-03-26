(in-package #:cl-data-structures.utils.clustering.k-means)


(-> k-means (vector positive-fixnum non-negative-single-float
                    &key
                    (:silhouette-sample-size (or null positive-fixnum))
                    (:silhouette-sample-count (or null positive-fixnum))
                    (:iterations (or null positive-fixnum))
                    (:value-key (or null function)))
    cl-ds.utils.cluster:clustering-result)
(defun k-means (data number-of-medoids distortion-epsilon
                &rest all
                &key silhouette-sample-size iterations
                  silhouette-sample-count value-key)
  (declare (ignore value-key silhouette-sample-size
                   iterations silhouette-sample-count))
  (iterate
    (with state = (make-state data number-of-medoids
                              distortion-epsilon all))
    (with iterations = (read-iterations state))
    (for i from 0)
    (while (or (null iterations) (< i iterations)))
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
