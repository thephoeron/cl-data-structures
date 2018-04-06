(in-package #:cl-data-structures.metric-space.egnat)


(defclass egnat-metric-set (cl-ds.ms:metric-space-set
                            cl-ds.common.egnat:fundamental-egnat-container)
  ())


(defclass mutable-egnat-metric-set (cl-ds.ms:mutable-metric-space-set
                                    cl-ds.common.egnat:mutable-egnat-container)
  ())


(defmethod cl-ds.common.egnat:same ((container egnat-metric-set)
                                    bucket item)
  (cl-ds.ms:same container bucket item))


(defmethod cl-ds.common.egnat:distance ((container egnat-metric-set)
                                        bucket item)
  (cl-ds.ms:distance container bucket item))


(defun make-mutable-egnat-metric-set (same-function distance-function distance-type
                                      &key (branching-factor 20) (node-size 50))
  (ensure-functionf same-function distance-function)
  (make 'egnat-metric-set
        :metric-fn distance-function
        :same-fn same-function
        :distance-type distance-type
        :branching-factor branching-factor
        :content-count-in-node node-size))
