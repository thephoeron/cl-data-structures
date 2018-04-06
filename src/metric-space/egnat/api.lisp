(in-package #:cl-data-structures.metric-space.egnat)


(defclass egnat-metric-set (cl-ds.ms:metric-space-set
                            cl-ds.common.egnat:fundamental-egnat-container)
  ())


(defclass mutable-egnat-metric-set (cl-ds.ms:mutable-metric-space-set
                                    cl-ds.common.egnat:mutable-egnat-container)
  ())
