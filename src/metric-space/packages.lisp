(in-package :cl-user)

(defpackage cl-data-structures.metric-space
  (:nicknames cl-ds.ms)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:distance
           #:metric-space-dictionary
           #:metric-space-set
           #:mutable-metric-space-dictionary
           #:mutable-metric-space-set))

(defpackage cl-data-structures.metric-space.egnat
  (:nicknames cl-ds.ms.egnat)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:make-mutable-egnat-metric-set
           #:mutable-egnat-metric-set
           #:egnat-metric-set))
