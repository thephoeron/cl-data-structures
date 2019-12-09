(cl:in-package #:cl-user)


(defpackage :cl-data-structures.sets
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sets)
  (:export
   #:fundamental-set
   #:mutable-set))


(defpackage cl-data-structures.sets.qp-trie
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sets.qp-trie)
  (:export
   #:empty-array-key
   #:fundamental-qp-trie-set
   #:make-mutable-qp-trie-set
   #:mutable-qp-trie-set))


(defpackage cl-data-structures.sets.skip-list
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sets.skip-list)
  (:export
   #:mutable-skip-list-set
   #:fundamental-skip-list-set
   #:make-mutable-skip-list-set))
