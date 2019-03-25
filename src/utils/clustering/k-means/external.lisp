(in-package #:cl-data-structures.utils.clustering.k-means)


(defun k-means (data medoids-count distortion-epsilon)
  (iterate
    (with state = (make-state data medoids-count
                               distortion-epsilon))
    (assign-data-points-to-medoids state)
    (select-new-medoids state)
    (for distortion = (distortion state))
    (for prev-distortion
         previous distortion
         initially nil)
    (while (or (null prev-distortion)
               (< (abs (- distortion prev-distortion))
                  distortion-epsilon )))
    (finally (obtain-result state))))
