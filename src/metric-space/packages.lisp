(in-package #:cl-user)


(defpackage :cl-data-structures.metric-space
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.ms)
  (:export
   #:distance
   #:same
   #:metric-space-dictionary
   #:metric-space-set
   #:mutable-metric-space-dictionary
   #:mutable-metric-space-set))


(defpackage :cl-data-structures.metric-space.egnat
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:cl-ds.utils
        #:metabang-bind #:cl-data-structures.common.hamt)
  (:nicknames #:cl-ds.ms.egnat)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:egnat-metric-set
   #:mutable-egnat-metric-set
   #:make-mutable-egnat-metric-set))
