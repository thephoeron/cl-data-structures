(cl:in-package #:cl-user)


(defpackage :cl-data-structures.streaming-algorithms
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sa)
  (:shadow cl:union)
  (:local-nicknames (#:hll #:cl-data-structures.streaming-algorithms.hyperloglog))
  (:export
   #:approximated-counts
   #:approximated-set-cardinality
   #:approximated-top-k
   #:bloom-filter
   #:clean-sketch
   #:fundamental-data-sketch
   #:gather-minhash-corpus
   #:hyperloglog-jaccard
   #:make-hash-array
   #:make-minhash
   #:minhash
   #:minhash-jaccard/double-float
   #:minhash-jaccard/fixnum
   #:minhash-jaccard/single-float
   #:union))
