(defpackage cl-data-structures.utils.metric
  (:use #:cl #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.utils.metric)
  (:export
   #:earth-mover-metric
   #:euclid-metric
   #:hausdorff-metric
   #:hellinger-metric
   #:levenshtein-metric))
