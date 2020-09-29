(in-package :cl-user)

(defpackage :cl-data-structures.sets
  (:nicknames cl-ds.sets)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:fundamental-set
           #:mutable-set))

(defpackage cl-data-structures.sets.qp-trie
  (:nicknames cl-ds.sets.qp-trie)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:empty-array-key
           #:fundamental-qp-trie-set
           #:make-mutable-qp-trie-set
           #:mutable-qp-trie-set))

(defpackage cl-data-structures.sets.skip-list
  (:nicknames cl-ds.sets.skip-list)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:mutable-skip-list-set
           #:make-mutable-skip-list-set))
