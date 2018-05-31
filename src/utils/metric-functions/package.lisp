(defpackage cl-data-structures.utils.metric
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds.utils.metric)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:hellinger-metric
   #:levenshtein-metric))
