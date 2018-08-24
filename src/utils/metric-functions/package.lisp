(defpackage cl-data-structures.utils.metric
  (:use #:cl #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.utils.metric)
  (:export
   #:hellinger-metric
   #:levenshtein-metric
   #:earth-mover-metric
   #:euclid-metric))
