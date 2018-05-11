(in-package #:cl-ds.utils.cluster)


(defun clear-improvements (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %improvements (adjust-array %improvements
                                      (length %cluster-contents)))
    (map-into %improvements (constantly nil))))


(defun improved-clusters-p (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (some #'identity %improvements)))


(defun clear-cluster-contents (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)))


(defun order-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %cluster-contents (sort %cluster-contents #'< :key #'first-elt))))


(defun choose-initial-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with generator = (cl-ds.utils:lazy-shuffle 0 (length %indexes)))
      (for cluster in-vector %cluster-contents)
      (for new-value = (funcall generator))
      (assert new-value)
      (setf (aref cluster 0) new-value))
    (order-medoids state)))


(defun medoid-p (state index)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (let* ((position (cl-ds.utils:lower-bound
                      (the (vector (vector non-negative-fixnum))
                           %cluster-contents)
                      (the non-negative-fixnum index)
                      #'<
                      :key #'first-elt))
           (cluster-count (length %cluster-contents)))
      (and (< position cluster-count)
           (= index (~> %cluster-contents
                        (aref index)
                        first-elt))))))


(defun closest-medoid (state index)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (unless (medoid-p state index)
      (iterate
        (declare (type number distance))
        (with result = 0)
        (for cluster in-vector %cluster-contents)
        (for i from 0)
        (for medoid = (aref cluster 0))
        (for distance = (cl-ds.utils:distance %distance-matrix index medoid))
        (minimize distance into mini)
        (when (= mini distance)
          (setf result i))
        (finally (return result))))))


(defun assign-data-points-to-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with assignments = (lparallel:pmap '(vector (or null fixnum))
                                          (curry #'closest-medoid state)
                                          %indexes))
      (for i in-vector %indexes)
      (for assignment in-vector assignments)
      (for medoid-p = (null assignment))
      (unless medoid-p
        (vector-push-extend i (aref %cluster-contents assignment))))))


(defun intra-cluster-distances (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (for c in-vector cluster)
      (sum
       (iterate
         (for k in-vector cluster)
         (when (eql c k)
           (next-iteration))
         (sum (cl-ds.utils:distance %distance-matrix
                                    c k)
              into sum)
         (finally (return (/ sum (length cluster))))) ; should be 1- length but it gets problematic for length = 1 so to keep it simple we are just a little bit incorrect here
       into sum)
      (finally (return (/ sum (length cluster)))))))


(defun inter-cluster-distances (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (for other-cluster in-vector %cluster-contents)
      (when (eq other-cluster cluster)
        (next-iteration))
      (minimize
       (iterate
         (for k in-vector cluster)
         (sum (iterate
                (for c in-vector other-cluster)
                (for distance = (cl-ds.utils:distance %distance-matrix
                                                      c k))
                (sum distance into sum)
                (finally (return (/ sum (length other-cluster)))))
              into sum)
         (finally (return (/ sum (length cluster)))))))))


(defun silhouette (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (flet ((map-distance (function)
             (lparallel:pmap 'vector
                             (curry function state)
                             %cluster-contents))
           (distance-difference (intra inter)
             (if (zerop (max intra inter))
                 0.0
                 (/ (- inter intra) (max intra inter)))))
      (map 'vector
           #'distance-difference
           (map-distance #'intra-cluster-distances)
           (map-distance #'inter-cluster-distances)))))


(defun choose-effective-medoid (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind (((:dflet swap-medoid (i))
            (rotatef (aref cluster i) (aref cluster 0)))
           ((:dflet total-distance-to-medoid ())
            (iterate
              (for i from 1 below (length cluster))
              (sum (cl-ds.utils:distance %distance-matrix
                                         (aref cluster 0)
                                         (aref cluster 1))))))
      (cl-ds.utils:optimize-value ((minimal-distance-to-medoid
                                    <
                                    (total-distance-to-medoid)))
        (iterate
          (for i from 1 below (length cluster))
          (swap-medoid i)
          (for distance = (total-distance-to-medoid))
          (minimal-distance-to-medoid distance)
          (for improved = (= distance minimal-distance-to-medoid))
          (unless improved
            (swap-medoid i))
          (for improved-something
               initially nil
               then (or improved improved-something))
          (finally (return improved-something)))))))


(defun choose-effective-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (lparallel:pmap-into %improvements
                         (curry #'choose-effective-medoid state)
                         %cluster-contents)
    (order-medoids state)))


(defun attempt-to-merge-clusters-above-threshold (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)))


(defun attempt-to-split-clusters-below-threshold (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with count-of-splitted = (cl-ds.utils:swap-if %cluster-contents
                                                     %split-threshold
                                                     :key #'length))
      (with cluster-count = (length %cluster-contents))
      (for index from (1- cluster-count) downto 0)
      (repeat count-of-splitted)
      (for cluster = (aref %cluster-contents index))
      (for cluster-size = (length cluster))
      (for estimated-size =
           (max 2 (round-to (/ cluster-size %number-of-medoids) 2)))
      (for number-of-medoids = (min %number-of-medoids cluster-size)))))
