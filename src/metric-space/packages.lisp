(in-package #:cl-user)


(defpackage :cl-data-structures.metric-space
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.ms)
  (:export))


(defpackage :cl-data-structures.metric-space.egnat
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:cl-ds.utils #:metabang-bind
        #:docstample #:docstample.mechanics #:cl-data-structures.common.hamt)
  (:nicknames #:cl-ds.ms.egnat)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export))
