(defpackage cl-data-structures.utils.clustering
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds.utils.cluster)
  (:shadowing-import-from #:iterate #:collecting #:sum #:into #:in)
  (:export
   #:clara
   #:clara-variable-number-of-medoids
   #:partition-around-medoids
   #:read-cluster-contents
   #:read-silhouette))
