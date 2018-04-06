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
