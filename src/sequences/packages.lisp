(in-package #:cl-user)


(defpackage :cl-data-structures.sequences
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.seqs)
  (:export
   #:abstract-sequence
   #:functional-sequence
   #:mutable-sequence
   #:transactional-sequence))


(defpackage :cl-data-structures.sequences.rrb-vector
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:cl-ds.utils #:metabang-bind
        #:docstample #:docstample.mechanics #:cl-data-structures.common.hamt)
  (:nicknames #:cl-ds.seqs.rrb)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:functional-rrb-vector
   #:mutable-rrb-vector
   #:transactional-rrb-vector))
