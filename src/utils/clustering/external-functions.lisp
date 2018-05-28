(in-package #:cl-ds.utils.cluster)


(-> partition-around-medoids (vector
                              cl-ds.utils:half-matrix
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
        (iterate
          (scan-for-clusters-of-invalid-size state)
          (while (unfinished-clusters-p state))
          (repeat %split-merge-attempts-count)
          (recluster-clusters-with-invalid-size state))))
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
  (let ((state (build-clara-clusters
                input-data number-of-medoids metric-type
                metric-fn sample-size sample-count
                :key key
                :select-medoids-attempts-count select-medoids-attempts-count
                :attempts attempts :split split :merge merge)))
    (assign-clara-data-to-medoids state)
    (replace-indexes-in-clusters-with-data state)
    (obtain-result state (read-silhouette state))))


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
  (assert (< 0 from to))
  (let ((vector (make-array (1+ (- to from)))))
    (iterate
      (for index from 0 below (length vector))
      (for i from from to to)
      (nest (setf (aref vector index))
            (let ((i i)))
            (bt:make-thread)
            (lambda ())
            (build-clara-clusters
             input-data i metric-type metric-fn
             sample-size sample-count
             :key key
             :select-medoids-attempts-count select-medoids-attempts-count
             :attempts attempts
             :split split
             :merge merge)))
    (iterate
      (with final = nil)
      (for thread in-vector vector)
      (for state = (bt:join-thread thread))
      (for mean-silhouette = (~> state access-silhouette mean))
      (maximize mean-silhouette into maximum)
      (when (= mean-silhouette maximum)
        (setf final state))
      (finally (assign-clara-data-to-medoids final)
               (replace-indexes-in-clusters-with-data final)
               (return (obtain-result state (access-silhouette state)))))))
