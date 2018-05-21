(defpackage cl-data-structures.utils.distance
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds.utils.distance)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:bhattacharyya-distance))
