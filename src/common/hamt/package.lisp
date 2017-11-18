(in-package #:cl-user)


(defpackage :cl-data-structures.common.hamt
  (:use #:common-lisp #:iterate #:serapeum #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.hamt)
  (:export
   #:hamt-container
   #:hash-node-whole-mask
   #:hash-node-to-masked-index
   #:hash-node-contains
   #:hash-node-contains-node
   #:hash-node-access
   #:hash-node-size
   #:go-down-on-path
   #:copy-node
   #:hash-node-replace-in-the-copy
   #:hash-node-insert-into-copy
   #:build-rehashed-node
   #:rebuild-rehashed-node
   #:mark-everything-as-modified
   #:transactional-rebuild-rehashed-node
   #:build-node
   #:hash-node-insert!
   #:hash-node-replace!
   #:hash-node-remove-from-the-copy
   #:hash-node-remove!
   #:rehash
   #:copy-on-write
   #:hash-node-content-modified
   #:set-modified
   #:hash-node-transactional-replace
   #:hash-node-transactional-insert
   #:hash-node-transactional-remove
   #:transactional-copy-on-write
   #:clear-modification-masks
   #:hash-node-deep-copy
   #:isolate-transactional-instance))
