(in-package #:cl-ds.utils.cluster)


(defun clear-unfinished-clusters (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %unfinished-clusters (adjust-array %unfinished-clusters
                                             (length %cluster-contents)
                                             :fill-pointer (length %cluster-contents)))
    (map-into %unfinished-clusters (constantly nil))))


(defun unfinished-clusters-p (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (some #'identity %unfinished-clusters)))


(defun clear-cluster-contents (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)
    (setf (fill-pointer %cluster-contents) %number-of-medoids)))


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
      (setf (aref cluster 0) (aref %indexes new-value)))
    (order-medoids state)))


(defun medoid-p (state index)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (let* ((position (cl-ds.utils:lower-bound
                      %cluster-contents
                      (the non-negative-fixnum index)
                      #'<
                      :key #'first-elt))
           (cluster-count (length %cluster-contents)))
      (and (< position cluster-count)
           (= index (~> %cluster-contents
                        (aref position)
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
  (declare (optimize (debug 3)))
  (if (~> cluster length zerop)
      0.0
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
                    (assert distance)
                    (sum distance into sum)
                    (finally (return (/ sum (length other-cluster)))))
                  into sum)
             (finally (return (/ sum (length cluster))))))))))


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


(-> choose-effective-medoid (pam-algorithm-state (vector t)) boolean)
(defun choose-effective-medoid (state cluster)
  (declare (optimize (speed 1)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind (((:dflet swap-medoid (i))
            (rotatef (aref cluster i) (aref cluster 0)))
           ((:dflet total-distance-to-medoid (&optional old-cost))
            (iterate
              (for i from 1 below (length cluster))
              (sum (cl-ds.utils:distance %distance-matrix
                                         (aref cluster 0)
                                         (aref cluster 1))
                   into sum)
              (unless (null old-cost)
                (while (< sum old-cost)))
              (finally (return sum))))
           (improved-something nil))
      (cl-ds.utils:optimize-value ((minimal-distance-to-medoid
                                    <
                                    (total-distance-to-medoid)))
        (iterate
          (for i from 1 below (length cluster))
          (swap-medoid i)
          (for distance = (total-distance-to-medoid
                           minimal-distance-to-medoid))
          (minimal-distance-to-medoid distance)
          (for improved = (= distance minimal-distance-to-medoid))
          (unless improved
            (swap-medoid i))
          (setf improved-something
                (not (null (or improved improved-something))))))
      improved-something)))


(defun choose-effective-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (lparallel:pmap-into %unfinished-clusters
                         (curry #'choose-effective-medoid state)
                         %cluster-contents)
    (order-medoids state)))


(defun build-pam-clusters (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (let ((clusters-ok
            (iterate
              (with attempts = (read-select-medoids-attempts-count state))
              (for i from 0)
              (unless (null attempts)
                (when (eql i attempts)
                  (finish)))
              (when (zerop (rem i 5))
                (choose-initial-medoids state))
              (assign-data-points-to-medoids state)
              (choose-effective-medoids state)
              (never (unfinished-clusters-p state))
              (clear-cluster-contents state)
              (clear-unfinished-clusters state))))
      (unless clusters-ok
        (warn "Could not find optimal clusters, continue with suboptimal clusters")
        (clear-cluster-contents state)
        (order-medoids state)
        (assign-data-points-to-medoids state)
        (choose-effective-medoids state)
        (clear-unfinished-clusters state)))))


(defun scan-for-clusters-of-invalid-size (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (clear-unfinished-clusters state)
    (map-into %unfinished-clusters
              (lambda (x)
                (not (< %merge-threshold
                        (length x)
                        %split-threshold)))
              %cluster-contents)))


(defun attempt-to-split-clusters-above-threshold (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with count-of-splitted = (cl-ds.utils:swap-if %cluster-contents
                                                     (curry #'<
                                                            %split-threshold)
                                                     :key #'length))
      (with cluster-count = (length %cluster-contents))
      (for index from (1- cluster-count) downto 0)
      (repeat count-of-splitted)
      (for cluster = (aref %cluster-contents index))
      (for fresh-state = (clone-state state
                                      :indexes cluster))
      (build-pam-clusters fresh-state)
      (map nil (lambda (x) (collect x into splitted-clusters at start))
           (read-cluster-contents fresh-state))
      (finally (decf (fill-pointer %cluster-contents) count-of-splitted)
               (map nil (rcurry #'vector-push-extend %cluster-contents)
                    splitted-clusters)))))


(defun attempt-to-merge-clusters-below-threshold (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind ((count-of-merged (cl-ds.utils:swap-if %cluster-contents
                                                 (curry #'>
                                                        %merge-threshold)
                                                 :key #'length))
           (cluster-count (length %cluster-contents))
           (count-for-merging (iterate
                                (for i from (1- cluster-count) downto 0)
                                (repeat count-of-merged)
                                (sum (~> %cluster-contents
                                         (aref i)
                                         length)))))
      (unless (zerop count-for-merging)
        (iterate
          (with index = 0)
          (with indexes = (make-array count-for-merging
                                      :element-type 'non-negative-fixnum))
          (for i from (1- cluster-count) downto 0)
          (repeat count-of-merged)
          (for cluster = (aref %cluster-contents i))
          (iterate
            (for value in-vector cluster)
            (setf (aref indexes index) value)
            (incf index))
          (finally
           (let ((fresh-state (clone-state state
                                           :indexes indexes)))
             (build-pam-clusters fresh-state)
             (decf (fill-pointer %cluster-contents) count-of-merged)
             (map nil
                  (rcurry #'vector-push-extend %cluster-contents)
                  (read-cluster-contents fresh-state)))))))))


(defun replace-indexes-in-cluster-with-data (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (map-into cluster (curry #'aref %input-data) cluster)))


(defun replace-indexes-in-clusters-with-data (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (lparallel:pmap-into %cluster-contents
                         (curry #'replace-indexes-in-cluster-with-data
                                state)
                         %cluster-contents)))


(defgeneric obtain-result (state silhouette))


(defmethod obtain-result ((state pam-algorithm-state) silhouette)
  (make 'clustering-result
        :cluster-contents (read-cluster-contents state)
        :silhouette silhouette))


(defun initialize-distance-matrix (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (setf %distance-matrix
          (cl-ds.utils:parallel-make-distance-matrix-from-vector
           %metric-type
           (lambda (a b)
             (funcall %metric-fn
                      (funcall %key (aref %input-data a))
                      (funcall %key (aref %input-data b))))
           %indexes
           :query-key (curry #'aref %index-mapping)))))


(defun draw-clara-sample (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (setf %all-indexes (shuffle %all-indexes))
    (setf %indexes (take %sample-size %all-indexes))
    (iterate
      (for k from 0)
      (for i in-vector %indexes)
      (setf (aref %index-mapping i) k))
    (clear-unfinished-clusters state)
    (clear-cluster-contents state)
    (initialize-distance-matrix state)))


(defun update-result-cluster (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (let* ((silhouette (silhouette state))
           (mean-silhouette (mean silhouette)))
      (when (> mean-silhouette %mean-silhouette)
        (setf %silhouette silhouette
              %mean-silhouette mean-silhouette
              %result-cluster-contents (map 'vector #'copy-array %cluster-contents))))))


(defun assign-clara-data-to-medoids (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)
    (order-medoids state)
    (iterate
      (for index in-vector %all-indexes)
      (unless (medoid-p state index)
        (iterate
          (with target = 0)
          (for j from 0)
          (for cluster in-vector %cluster-contents)
          (for distance = (funcall %metric-fn
                                   (~>> index
                                        (aref %input-data)
                                        (funcall %key))
                                   (~>> cluster
                                        first-elt
                                        (aref %input-data)
                                        (funcall %key))))
          (minimize distance into mini)
          (when (= distance mini)
            (setf target j))
          (finally (vector-push-extend index
                                       (aref %cluster-contents
                                             target))))))))
