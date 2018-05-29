(in-package #:cl-ds.utils.cluster)


(defclass pam-algorithm-state ()
  ((%input-data :initarg :input-data
                :reader read-input-data)
   (%number-of-medoids :initarg :number-of-medoids
                       :reader read-number-of-medoids)
   (%distance-matrix :initarg :distance-matrix
                     :reader read-distance-matrix)
   (%select-medoids-attempts-count :initarg :select-medoids-attempts-count
                                   :reader read-select-medoids-attempts-count
                                   :initform 20)
   (%split-merge-attempts-count :initarg :split-merge-attempts-count
                                :initform 0)
   (%split-threshold :initarg :split-threshold
                     :initform nil)
   (%merge-threshold :initarg :merge-threshold
                     :initform nil)
   (%unfinished-clusters :initarg :improvements)
   (%cluster-size :initarg :cluster-size)
   (%indexes :initarg :indexes
             :reader read-indexes)
   (%cluster-contents :initarg :cluster-contents
                      :reader read-cluster-contents)))


(defclass clara-algorithm-state (pam-algorithm-state)
  ((%result-cluster-contents :initform nil
                             :accessor access-result-cluster-contents)
   (%all-indexes :reader read-all-indexes)
   (%metric-type :initarg :metric-type)
   (%metric-fn :initarg :metric-fn)
   (%sample-count :initarg :sample-count)
   (%key :initarg :key)
   (%index-mapping :initform nil
                   :reader read-index-mapping)
   (%sample-size :initarg :sample-size)
   (%mean-silhouette :initform -10 ;silhouette is bound by definition in -1 to +1
                     :accessor access-mean-silhouette)
   (%silhouette :initform nil
                :accessor access-silhouette)))


(defclass clustering-result ()
  ((%cluster-contents :initarg :cluster-contents
                      :reader read-cluster-contents)
   (%silhouette :initarg :silhouette
                :reader read-silhouette)))


(cl-ds.utils:define-list-of-slots pam-algorithm-state
  %input-data %number-of-medoids %distance-matrix
  %split-merge-attempts-count %split-threshold
  %merge-threshold %unfinished-clusters
  %select-medoids-attempts-count
  %cluster-contents %indexes %cluster-size)


(cl-ds.utils:define-list-of-slots clara-algorithm-state
  %input-data %number-of-medoids %distance-matrix
  %split-merge-attempts-count %split-threshold
  %merge-threshold %unfinished-clusters %metric-fn
  %metric-type %select-medoids-attempts-count
  %sample-size %cluster-contents %silhouette %key
  %index-mapping %sample-count %all-indexes %indexes
  %cluster-size %result-cluster-contents %mean-silhouette)


(defun restart-pam (object)
  (declare (optimize (safety 3) (debug 3)))
  (cl-ds.utils:with-slots-for (object pam-algorithm-state)
    (if (zerop %split-merge-attempts-count)
        (progn (assert (null %merge-threshold))
               (assert (null %split-threshold)))
        (assert (< 0 %merge-threshold %split-threshold)))
    (macrolet ((slot-initialized-p (slot)
                 `(and (slot-boundp object ',slot)
                       (not (null ,slot)))))
      (unless (slot-initialized-p %indexes)
        (setf %indexes (coerce (~> %input-data length iota)
                               '(vector non-negative-fixnum))))
      (let ((length (length %indexes)))
        (setf %number-of-medoids
              (if (slot-initialized-p %number-of-medoids)
                  (max (min %number-of-medoids length) 1)
                  length))
        (if (slot-initialized-p %cluster-size)
            (assert (< 0 %cluster-size))
            (setf %cluster-size (max 2 (round-to (/ length %number-of-medoids)
                                                 2))))
        (unless (slot-initialized-p %cluster-contents)
          (setf %cluster-contents (make-array %number-of-medoids
                                              :adjustable t
                                              :fill-pointer %number-of-medoids))
          (map-into %cluster-contents
                    (lambda () (make-array %cluster-size :adjustable t
                                                    :fill-pointer 1))))
        (unless (slot-initialized-p %unfinished-clusters)
          (setf %unfinished-clusters
                (make-array %number-of-medoids
                            :element-type 'boolean
                            :adjustable t
                            :fill-pointer %number-of-medoids
                            :initial-element nil)))))))


(defmethod initialize-instance :after ((object pam-algorithm-state)
                                       &key &allow-other-keys)
  (restart-pam object))


(defmethod initialize-instance :after ((object clara-algorithm-state)
                                       &key &allow-other-keys)
  (cl-ds.utils:with-slots-for (object clara-algorithm-state)
    (setf %all-indexes %indexes)
    (setf %sample-size (min (length %indexes) %sample-size))
    (setf %index-mapping (make-array (length %indexes)
                                     :element-type 'fixnum))))


(defun clone-state (state &key indexes)
  (lret ((result (make 'pam-algorithm-state
                       :input-data (read-input-data state)
                       :indexes indexes
                       :clusters nil
                       :unfinished-clusters nil
                       :distance-matrix (read-distance-matrix state))))
    (assert (not (emptyp (read-indexes state))))))
