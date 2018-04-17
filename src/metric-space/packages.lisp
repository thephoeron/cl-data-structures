(in-package #:cl-user)


(defpackage :cl-data-structures.metric-space
  (:use #:common-lisp #:serapeum
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.ms)
  (:export
   #:distance
   #:metric-space-dictionary
   #:metric-space-set
   #:mutable-metric-space-dictionary
   #:mutable-metric-space-set
   #:same))


(defpackage :cl-data-structures.metric-space.egnat
  (:use #:common-lisp #:iterate #:alexandria #:serapeum
        #:metabang-bind #:cl-data-structures.common.hamt)
  (:nicknames #:cl-ds.ms.egnat)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:make-mutable-egnat-metric-set
   #:mutable-egnat-metric-set
   #:egnat-metric-set))
