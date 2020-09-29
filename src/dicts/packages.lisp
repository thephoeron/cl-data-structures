(in-package :cl-user)

(defpackage cl-data-structures.dicts
  (:nicknames cl-ds.dicts)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:fundamental-dictionary
           #:fundamental-hashing-dictionary
           #:fundamental-sparse-vector
           #:bucket
           #:find-content
           #:functional-sparse-vector
           #:mutable-sparse-vector
           #:transactional-sparse-vector
           #:functional-dictionary
           #:functional-hashing-dictionary
           #:hashing-dictionary
           #:lazy-dictionary
           #:lazy-hashing-dictionary
           #:make-bucket
           #:mutable-dictionary
           #:mutable-hashing-dictionary
           #:read-equal-fn
           #:read-hash-fn
           #:transactional-dictionary
           #:transactional-hashing-dictionary))

(defpackage cl-data-structures.dicts.hamt
  (:nicknames cl-ds.dicts.hamt)
  (:use c2cl cl-data-structures.aux-package cl-data-structures.common.hamt cl-data-structures.common.abstract)
  (:export #:functional-hamt-dictionary
           #:hamt-dictionary
           #:hamt-dictionary-at
           #:hamt-dictionary-size
           #:make-functional-hamt-dictionary
           #:make-transactional-hamt-dictionary
           #:make-mutable-hamt-dictionary
           #:mutable-hamt-dictionary
           #:read-max-depth
           #:transactional-hamt-dictionary))

(defpackage cl-data-structures.dicts.srrb
  (:nicknames cl-ds.dicts.srrb)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:access-shift
           #:access-tree
           #:access-tree-size
           #:access-tail-mask
           #:access-tail
           #:access-tree-index-bound
           #:functional-sparse-rrb-vector
           #:functional-sparse-rrb-vector-grow
           #:insert-tail
           #:insert-tail!
           #:shift-for-position
           #:make-functional-sparse-rrb-vector
           #:make-mutable-sparse-rrb-vector
           #:make-transactional-sparse-rrb-vector
           #:mutable-sparse-rrb-vector
           #:mutable-sparse-rrb-vector-grow
           #:scan-index-bound
           #:sparse-rrb-vector-at
           #:transactional-insert-tail!
           #:transactional-sparse-rrb-vector
           #:transactional-sparse-rrb-vector-grow
           #:access-index-bound))
