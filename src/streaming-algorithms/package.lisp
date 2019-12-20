(in-package #:cl-user)


(defpackage :cl-data-structures.streaming-algorithms
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sa)
  (:shadow cl:union)
  (:export
   #:approximated-counts
   #:bloom-filter
   #:clean-sketch
   #:fundamental-data-sketch
   #:make-hash-array
   #:union
   #:gather-minhash-corpus
   #:minhash
   #:approximated-set-cardinality))
