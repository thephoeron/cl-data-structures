(in-package #:cl-ds.utils.cluster)


(defclass pam-algorithm-state ()
  ((%input-data :initarg :input-data)
   (%number-of-medoids :initarg :number-of-medoids)
   (%distance-matrix :initarg :distance-matrix)
   (%split-merge-attempts-count :initarg :split-merge-attempts-count
                                :initform 0)
   (%split-threshold :initarg :split-threshold
                     :initform nil)
   (%merge-threshold :initarg :merge-threshold
                     :initform nil)
   (%improvements :initarg :improvements)
   (%clusters :initarg :clusters)
   (%cluster-size :initarg :cluster-size)
   (%indexes :initarg :cluster-size)
   (%cluster-contents :initarg :cluster-contents)))


(cl-ds.utils:define-list-of-slots 'pam-algorithm-state
  %input-data %number-of-medoids %distance-matrix
  %split-merge-attempts-count %split-threshold
  %merge-threshold %improvements %cluster
  %indexes %cluster-size %cluster-contents)


(defmethod initialize-instance :after ((object pam-algorithm-state)
                                       &key &allow-other-keys)
  (cl-ds.utils:with-slots-for (object pam-algorithm-state)
    (if (zerop %split-merge-attempts-count)
        (assert (< 0 %merge-threshold %split-threshold))
        (progn (assert (null %merge-threshold))
               (assert (null %split-threshold))))
    (let ((length (length %input-data)))
      (setf %number-of-medoids (min %number-of-medoids length))
      (unless (slot-boundp object '%indexes)
        (setf %indexes (coerce (iota length)
                               '(vector non-negative-fixnum))))
      (if (slot-boundp object '%cluster-size)
          (assert (< 0 %cluster-size))
          (setf %cluster-size (max 2 (round-to (/ length %number-of-medoids)
                                               2))))
      (unless (slot-boundp object '%cluster-contents)
        (setf %cluster-contents (make-array %number-of-medoids))
        (map-into %cluster-contents
                  (lambda () (make-array %cluster-size :adjustable t
                                                       :fill-pointer 1))))
      (unless (slot-boundp object '%improvements)
        (setf %improvements (make-array %number-of-medoids
                                        :element-type 'boolean
                                        :adjustable t
                                        :fill-pointer %number-of-medoids
                                        :initial-element nil))))))
