(in-package #:cl-user)


(defpackage :cl-data-structures.clustering
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in #:into #:sum)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.cluster)
  (:export
   #:clara
   #:clara-variable-number-of-medoids))
