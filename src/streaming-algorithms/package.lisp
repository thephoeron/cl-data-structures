(in-package #:cl-user)


(defpackage :cl-data-structures.streaming-algorithms
  (:use #:common-lisp #:serapeum #:cl-ds.utils #:alexandria
        #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.sa)
  (:export
   #:estimated-set-cardinality))
