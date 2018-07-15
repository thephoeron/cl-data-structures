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
    (setf (fill-pointer %cluster-contents) %number-of-medoids)
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
  (declare (optimize (speed 3) (safety 1)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (unless (medoid-p state index)
      (iterate
        (declare (type number distance))
        (with result = 0)
        (for cluster in-vector %cluster-contents)
        (for i from 0)
        (for medoid = (aref cluster 0))
        (for distance = (cl-ds.utils:mref %distance-matrix index medoid))
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


(-> intra-cluster-distances (pam-algorithm-state vector) single-float)
(defun intra-cluster-distances (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (for c in-vector cluster)
      (sum
       (iterate
         (for k in-vector cluster)
         (when (eql c k)
           (next-iteration))
         (sum (cl-ds.utils:mref %distance-matrix
                                c k)
              into sum)
         (finally (return (/ sum (length cluster))))) ; should be 1- length but it gets problematic for length = 1 so to keep it simple we are just a little bit incorrect here
       into sum)
      (finally (return (coerce (/ sum (length cluster))
                               'single-float))))))


(-> sum-distance-to-element (pam-algorithm-state non-negative-fixnum vector)
    single-float)
(defun sum-distance-to-element (state element cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
    (iterate
      (for c in-vector cluster)
      (for distance = (cl-ds.utils:mref %distance-matrix
                                        (the fixnum c)
                                        (the fixnum element)))
      (sum distance into sum)
      (finally (return (coerce (/ sum (length cluster))
                               'single-float))))))


(-> inter-cluster-distances (pam-algorithm-state vector) single-float)
(defun inter-cluster-distances (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (cl-ds.utils:optimize-value ((mini < 0.0))
      (iterate
        (for other-cluster in-vector %cluster-contents)
        (when (eq other-cluster cluster)
          (next-iteration))
        (mini (iterate
                (for k in-vector cluster)
                (sum (sum-distance-to-element state k other-cluster)
                     into sum)
                (finally (return (coerce (/ sum (length cluster))
                                         'single-float)))))))))


(defun silhouette (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (flet ((map-distance (function)
             (lparallel:pmap 'vector
                             (curry function state)
                             %cluster-contents))
           (distance-difference (intra inter)
             (if (zerop (max intra inter))
                 -1.0
                 (/ (- inter intra) (max intra inter)))))
      (map '(vector number)
           #'distance-difference
           (map-distance #'intra-cluster-distances)
           (map-distance #'inter-cluster-distances)))))


(-> choose-effective-medoid (pam-algorithm-state (vector t)) boolean)
(defun choose-effective-medoid (state cluster)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind (((:dflet swap-medoid (i))
            (declare (type non-negative-fixnum i))
            (rotatef (aref cluster i) (aref cluster 0)))
           ((:dflet total-distance-to-medoid (&optional old-cost))
            (iterate
              (for i from 1 below (length cluster))
              (for distance = (cl-ds.utils:mref %distance-matrix
                                                (the fixnum (aref cluster 0))
                                                (the fixnum (aref cluster i))))
              (assert distance)
              (sum distance into sum)
              (unless (null old-cost)
                (while (<= sum old-cost)))
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
    (assert (eql (length %unfinished-clusters)
                 (length %cluster-contents)))
    (lparallel:pmap-into %unfinished-clusters
                         (curry #'choose-effective-medoid state)
                         %cluster-contents)
    (order-medoids state)))


(defun scan-for-clusters-of-invalid-size (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (clear-unfinished-clusters state)
    (map-into %unfinished-clusters
              (lambda (x)
                (not (< %merge-threshold
                        (length x)
                        %split-threshold)))
              %cluster-contents)))


(defun build-pam-clusters (state &optional split-merge)
  (declare (optimize (speed 3)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (flet ((split-merge ()
             (when split-merge
               (unless (null %split-merge-attempts-count)
                 (iterate
                   (scan-for-clusters-of-invalid-size state)
                   (while (unfinished-clusters-p state))
                   (repeat %split-merge-attempts-count)
                   (recluster-clusters-with-invalid-size state))))))
      (let ((expired-attempts-limits
              (iterate
                (with attempts = %select-medoids-attempts-count)
                (for i from 0)
                (unless (null attempts)
                  (unless (< i attempts)
                    (leave t)))
                (when (zerop (rem i 5))
                  (clear-cluster-contents state)
                  (choose-initial-medoids state)
                  (order-medoids state)
                  (assign-data-points-to-medoids state))
                (choose-effective-medoids state)
                (always (unfinished-clusters-p state))
                (clear-cluster-contents state)
                (clear-unfinished-clusters state))))
        (when expired-attempts-limits
          (clear-cluster-contents state)
          (order-medoids state)
          (assign-data-points-to-medoids state)
          (split-merge)
          (clear-unfinished-clusters state))))))


(defun fill-reclustering-index-vector (state indexes count-of-eliminated)
  (declare (optimize (speed 3) (safety 1)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with position = 0)
      (for i from (~> %cluster-contents length 1-) downto 0)
      (repeat count-of-eliminated)
      (for cluster = (aref %cluster-contents i))
      (iterate
        (for value in-vector cluster)
        (setf (aref indexes position) value)
        (incf position))
      (finally
       (assert (eql position (length indexes)))
       (assert (iterate
                 (with data = (sort indexes #'<))
                 (for i from 1 below (length data))
                 (for a = (aref data i))
                 (for p-a = (aref data (1- i)))
                 (assert (not (eql a p-a)))
                 (never (eql a p-a)))))))
  indexes)


(defun prepare-reclustering-index-vector (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind ((count-of-eliminated (cl-ds.utils:swap-if
                                 %cluster-contents
                                 (lambda (x)
                                   (not (< %merge-threshold
                                           x
                                           %split-threshold)))
                                 :key #'length))
           (count-of-elements (iterate
                                (for i
                                     from (~> %cluster-contents length 1-)
                                     downto 0)
                                (repeat count-of-eliminated)
                                (sum (~> %cluster-contents
                                         (aref i)
                                         length))))
           ((:dflet expected-cluster-count ())
            (round (/ count-of-elements
                      (/ (+ %split-threshold %merge-threshold)
                         2)))))
      (iterate
        (while (zerop (expected-cluster-count)))
        (until (eql count-of-eliminated (length %cluster-contents)))
        (incf count-of-eliminated)
        (incf count-of-elements (~>> (length %cluster-contents)
                                     (- _ count-of-eliminated)
                                     (aref %cluster-contents)
                                     length)))
      (values
       (fill-reclustering-index-vector
        state
        (make-array count-of-elements :element-type 'non-negative-fixnum)
        count-of-eliminated)
       count-of-eliminated
       (expected-cluster-count)))))


(defun recluster-clusters-with-invalid-size (state)
  (declare (optimize (speed 2)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %cluster-contents (shuffle %cluster-contents))
    (bind (((:values indexes count-of-eliminated expected-cluster-count)
            (prepare-reclustering-index-vector state))
           (fresh-state (make
                         'pam-algorithm-state
                         :indexes indexes
                         :distance-matrix %distance-matrix
                         :split-threshold %split-threshold
                         :number-of-medoids expected-cluster-count
                         :select-medoids-attempts-count %select-medoids-attempts-count
                         :merge-threshold %merge-threshold
                         :split-merge-attempts-count %split-merge-attempts-count
                         :input-data %input-data)))
      (build-pam-clusters fresh-state nil)
      (decf (fill-pointer %cluster-contents) count-of-eliminated)
      (map nil
           (rcurry #'vector-push-extend %cluster-contents)
           (access-cluster-contents fresh-state))
      (order-medoids state))))


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
        :cluster-contents (access-cluster-contents state)
        :silhouette silhouette))


(defun index-mapping-function (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (let ((index-mapping %index-mapping))
      (declare (type (simple-array non-negative-fixnum (*)) %index-mapping))
      (lambda (x)
        (declare (optimize (speed 3)
                           (safety 0)
                           (debug 0)))
        (aref index-mapping x)))))


(defun initialize-distance-matrix (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (setf %distance-matrix
          (cl-ds.utils:parallel-make-distance-matrix-from-vector
           %metric-type
           %metric-fn
           (lparallel:pmap 'vector
                           (compose %key (curry #'aref %input-data))
                           %indexes)
           :query-key (index-mapping-function state)))
    (cl-progress-bar:update 1)))


(defun draw-clara-sample (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (setf %all-indexes (shuffle %all-indexes)
          %indexes (take %sample-size %all-indexes))
    (assert (<= (length %indexes)
                (length %index-mapping)))
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
              %result-cluster-contents (map 'vector
                                            #'copy-array
                                            %cluster-contents))))))


(defun assign-clara-data-to-medoids (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)
    (order-medoids state)
    (bind ((medoids (lparallel:pmap
                     'vector
                     (lambda (x)
                       (~>> x first-elt
                            (aref %input-data)
                            (funcall %key)))
                     %cluster-contents))
           (cluster-mutex (map-into (copy-array %cluster-contents)
                                    #'bt:make-lock)))
      (cl-data-structures.utils:with-rebind (cl-progress-bar:*progress-bar*)
        (lparallel:pmap
         nil
         (lambda (index)
           (cl-data-structures.utils:rebind
            (unless (medoid-p state index)
              (iterate
                (with some-data = (~>> index
                                       (aref %input-data)
                                       (funcall %key)))
                (with target = 0)
                (for j from 0)
                (for medoid in-vector medoids)
                (for distance = (funcall %metric-fn
                                         medoid
                                         some-data))
                (minimize distance into mini)
                (when (= distance mini)
                  (setf target j))
                (finally
                 (bt:with-lock-held ((aref cluster-mutex target))
                   (vector-push-extend index
                                       (aref %cluster-contents target))))))
            (cl-progress-bar:update 1)))
         %all-indexes)))))


(defun build-clara-clusters (input-data
                             number-of-medoids
                             metric-type
                             metric-fn
                             sample-size
                             sample-count
                             &key
                               (key #'identity)
                               (select-medoids-attempts-count 50)
                               (attempts 0)
                               split
                               merge)
  (when (emptyp input-data)
    (return-from build-clara-clusters
      (empty-clustering-result)))
  (let ((state (make 'clara-algorithm-state
                     :number-of-medoids number-of-medoids
                     :input-data input-data
                     :split-merge-attempts-count attempts
                     :select-medoids-attempts-count select-medoids-attempts-count
                     :split-threshold split
                     :key key
                     :metric-fn metric-fn
                     :sample-count sample-count
                     :metric-type metric-type
                     :sample-size sample-size
                     :merge-threshold merge)))
    (cl-ds.utils:with-slots-for (state clara-algorithm-state)
      (iterate
        (repeat %sample-count)
        (draw-clara-sample state)
        (build-pam-clusters state t)
        (update-result-cluster state)
        (cl-progress-bar:update 1))
      (setf %cluster-contents %result-cluster-contents)
      (assert %silhouette)
      state)))
