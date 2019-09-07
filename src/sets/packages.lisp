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
   #:mutable-qp-trie-set))
