(in-package #:cl-ds.utils.cluster)


(-> partition-around-medoids (vector
                              cl-ds.utils:distance-matrix
                              positive-fixnum
                              &key
                              (:select-medoids-attempts-count (or null positive-fixnum))
                              (:attempts non-negative-fixnum)
                              (:split (or null positive-fixnum))
                              (:merge (or null positive-fixnum)))
    t)
(defun partition-around-medoids (input-data
                                 distance-matrix
                                 number-of-medoids
                                 &key
                                   (select-medoids-attempts-count 50)
                                   (attempts 0)
                                   split
                                   merge)
  (when (or (zerop (length input-data)))
    (error "Can't cluster because there is no data"))
  (let ((state (make 'pam-algorithm-state
                     :number-of-medoids number-of-medoids
                     :input-data input-data
                     :distance-matrix distance-matrix
                     :split-merge-attempts-count attempts
                     :select-medoids-attempts-count select-medoids-attempts-count
                     :split-threshold split
                     :merge-threshold merge)))
    (cl-ds.utils:with-slots-for (state pam-algorithm-state)
      (build-pam-clusters state)
      (unless (null %split-merge-attempts-count)
        (clear-unfinished-clusters state)
        (iterate
          (scan-for-clusters-of-invalid-size state)
          (while (unfinished-clusters-p state))
          (repeat %split-merge-attempts-count)
          (attempt-to-split-clusters-above-threshold state)
          (attempt-to-merge-clusters-below-threshold state))))
    (let ((silhouette (silhouette state)))
      (replace-indexes-in-clusters-with-data state)
      (obtain-result state silhouette))))


(-> clara (vector
           positive-fixnum
           (or symbol list)
           function
           positive-fixnum
           positive-fixnum
           &key
           (:key function)
           (:select-medoids-attempts-count (or null positive-fixnum))
           (:attempts non-negative-fixnum)
           (:split (or null positive-fixnum))
           (:merge (or null positive-fixnum)))
    t)
(defun clara (input-data
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
  (declare (optimize (debug 3)))
  (when (or (zerop (length input-data)))
    (error "Can't cluster because there is no data"))
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
        (build-pam-clusters state)
        (unless (null %split-merge-attempts-count)
          (clear-unfinished-clusters state)
          (iterate
            (scan-for-clusters-of-invalid-size state)
            (while (unfinished-clusters-p state))
            (repeat %split-merge-attempts-count)
            (attempt-to-split-clusters-above-threshold state)
            (attempt-to-merge-clusters-below-threshold state)))
        (update-result-cluster state))
      (setf %cluster-contents %result-cluster-contents)
      (assign-clara-data-to-medoids state)
      (replace-indexes-in-clusters-with-data state)
      (assert %silhouette)
      (obtain-result state %silhouette))))


(defun clara-variable-number-of-medoids (input-data
                                         metric-type
                                         metric-fn
                                         sample-size
                                         sample-count
                                         from to
                                         &key
                                           (key #'identity)
                                           (select-medoids-attempts-count 50)
                                           (attempts 0)
                                           split
                                           merge)
  (declare (optimize (debug 3)))
  (assert (< 0 from to))
  (iterate
    (with final = nil)
    (for i from from to to)
    (for result = (clara input-data i metric-type metric-fn sample-size sample-count
                         :key key :select-medoids-attempts-count select-medoids-attempts-count
                         :attempts attempts :split split :merge merge))
    (for mean-silhouette = (~> result read-silhouette mean))
    (maximize mean-silhouette into maximum)
    (when (= mean-silhouette maximum)
      (setf final result))
    (finally (return result))))
