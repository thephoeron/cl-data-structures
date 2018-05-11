(in-package #:cl-ds.utils.cluster)


(-> partition-around-medoids (vector
                              non-negative-fixnum
                              cl-ds.utils:distance-matrix
                              non-negative-fixnum
                              &key
                              (:split (or null positive-fixnum))
                              (:merge (or null positive-fixnum)))
    (vector vector))
(defun partition-around-medoids (input-data number-of-medoids
                                 distance-matrix attempts &key split merge)
  (when (or (zerop (length input-data))
            (zerop number-of-medoids))
    (return-from partition-around-medoids
      (make-array 0 :element-type 'vector)))
  (let ((state (make-instance 'pam-algorithm-state
                              :number-of-medoids number-of-medoids
                              :input-data input-data
                              :distance-matrix distance-matrix
                              :split-merge-attempts-count attempts
                              :split-threshold split
                              :merge-threshold merge)))
    (build-pam-clusters state)
    (unless (null %split-merge-attempts-count)
      (clear-unfinished-clusters state)
      (iterate
        (repeat %split-merge-attempts-count)
        (attempt-to-split-clusters-above-threshold state)
        (attempt-to-merge-clusters-below-threshold state)
        (while (unfinished-clusters-p state))
        (clear-improvements state)))
    (obtain-result state)))
