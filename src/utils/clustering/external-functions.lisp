(in-package #:cl-ds.utils.cluster)


(-> partition-around-medoids (vector
                              cl-ds.utils:half-matrix
                              positive-fixnum
                              &key
                              (:select-medoids-attempts-count (or null positive-fixnum))
                              (:attempts non-negative-fixnum)
                              (:split (or null positive-fixnum))
                              (:merge (or null positive-fixnum)))
    clustering-result)
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
    (build-pam-clusters state t)
    (assign-data-points-to-medoids state)
    (let ((silhouette (silhouette state)))
      (replace-indexes-in-clusters-with-data state)
      (the clustering-result
           (obtain-result state
                          silhouette)))))


(-> clara (vector
           positive-fixnum
           (or symbol list)
           (or symbol function)
           positive-fixnum
           positive-fixnum
           &key
           (:key (or symbol function))
           (:select-medoids-attempts-count (or null positive-fixnum))
           (:attempts non-negative-fixnum)
           (:split (or null positive-fixnum))
           (:merge (or null positive-fixnum)))
    clustering-result)
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
  (let ((state nil))
    (cl-progress-bar:with-progress-bar
        ((* 2 sample-count)
         "Clustering data set of size ~a using CLARA algorithm with ~a samples of size ~a."
         (length input-data)
         sample-count
         sample-size)
      (setf state (build-clara-clusters
                   input-data number-of-medoids metric-type
                   (ensure-function metric-fn) sample-size sample-count
                   :key (ensure-function key)
                   :select-medoids-attempts-count select-medoids-attempts-count
                   :attempts attempts :split split :merge merge)))
    (cl-progress-bar:with-progress-bar
        ((length input-data)
         "Assigning data set to ~a clusters."
         (length (access-result-cluster-contents state)))
      (assign-clara-data-to-medoids state))
    (replace-indexes-in-clusters-with-data state)
    (obtain-result state (access-silhouette state))))


(-> clara-variable-number-of-medoids (vector
                                      (or symbol list)
                                      (or symbol function)
                                      positive-fixnum
                                      positive-fixnum
                                      positive-fixnum
                                      positive-fixnum
                                      &key
                                      (:key (or symbol function))
                                      (:select-medoids-attempts-count (or null positive-fixnum))
                                      (:attempts non-negative-fixnum)
                                      (:split (or null positive-fixnum))
                                      (:merge (or null positive-fixnum)))
    clustering-result)
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
  (let ((vector (make-array (1+ (- to from))))
        (metric-fn (ensure-function metric-fn))
        (key (ensure-function key))
        (final nil))
    (cl-progress-bar:with-progress-bar
        ((* (* 2 sample-count) (- (1+ to) from))
         %all-indexes
         "Clustering data set of size ~a using CLARA algorithm searching for optimal medoids count (between ~a and ~a)."
         (length input-data)
         from to)
      (iterate
        (with progress-bar = cl-progress-bar:*progress-bar*)
        (for index from 0 below (length vector))
        (for i from from to to)
        (nest (setf (aref vector index))
              (let ((i i)))
              (bt:make-thread)
              (lambda ())
              (let ((cl-progress-bar:*progress-bar* progress-bar)))
              (build-clara-clusters
               input-data i metric-type metric-fn
               sample-size sample-count
               :key key
               :select-medoids-attempts-count select-medoids-attempts-count
               :attempts attempts
               :split split
               :merge merge)))
      (iterate
        (for thread in-vector vector)
        (for state = (bt:join-thread thread))
        (for mean-silhouette = (~> state access-silhouette mean))
        (maximize mean-silhouette into maximum)
        (when (= mean-silhouette maximum)
          (setf final state))))
    (cl-progress-bar:with-progress-bar
        ((length input-data)
         "Assigning data set to ~a clusters."
         (length (access-result-cluster-contents final)))
      (assign-clara-data-to-medoids final))
    (replace-indexes-in-clusters-with-data final)
    (obtain-result final (access-silhouette final))))
