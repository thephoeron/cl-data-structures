(in-package #:cl-user)


(defpackage :cl-data-structures.streaming-algorithms
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sa)
  (:export
   #:approximated-set-cardinality
   #:approximated-counts
   #:bloom-filter
   #:make-hash-array))
