(in-package :cl-user)

(defpackage cl-data-structures.streaming-algorithms
  (:nicknames cl-ds.sa)
  (:use c2cl cl-data-structures.aux-package)
  (:shadow cl:union)
  (:export #:approximated-counts
           #:approximated-set-cardinality
           #:approximated-top-k
           #:bloom-filter
           #:clean-sketch
           #:fundamental-data-sketch
           #:gather-minhash-corpus
           #:hyperloglog-jaccard
           #:make-hash-array
           #:make-minhash
           #:make-one-bit-minhash
           #:minhash
           #:minhash-jaccard/double-float
           #:minhash-jaccard/fixnum
           #:minhash-jaccard/single-float
           #:one-bit-minhash-jaccard/single-float
           #:one-bit-minhash-jaccard/double-float
           #:one-bit-minhash-jaccard/fixnum
           #:union))
