(in-package :cl-user)

(defpackage cl-data-structures.sequences
  (:nicknames cl-ds.seqs)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:fundamental-sequence
           #:functional-sequence
           #:mutable-sequence
           #:transactional-sequence))

(defpackage cl-data-structures.sequences.rrb-vector
  (:nicknames cl-ds.seqs.rrb)
  (:use c2cl cl-data-structures.common.rrb cl-data-structures.aux-package)
  (:export #:functional-rrb-vector
           #:make-functional-rrb-vector
           #:make-mutable-rrb-vector
           #:make-transactional-rrb-vector
           #:mutable-rrb-vector
           #:transactional-rrb-vector))
