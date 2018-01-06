(in-package #:cl-user)


(defpackage :cl-data-structures.common.hamt
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind
        #:cl-data-structures.common.abstract)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.hamt)
  (:export
   #:+depth+
   #:+hash-level+
   #:+maximum-children-count+
   #:access-root
   #:access-size
   #:build-node
   #:build-rehashed-node
   #:clear-modification-masks
   #:copy-node
   #:copy-on-write
   #:get-range-key-function
   #:go-down-on-path
   #:hamt-container
   #:hash-do
   #:hash-node-access
   #:hash-node-contains
   #:hash-node-contains-node
   #:hash-node-content-modified
   #:hash-node-deep-copy
   #:hash-node-insert!
   #:hash-node-insert-into-copy
   #:hash-node-p
   #:hash-node-remove!
   #:hash-node-remove-from-the-copy
   #:hash-node-replace!
   #:hash-node-replace-in-the-copy
   #:hash-node-size
   #:hash-node-to-masked-index
   #:hash-node-transactional-insert
   #:hash-node-transactional-remove
   #:hash-node-transactional-replace
   #:hash-node-whole-mask
   #:new-cell
   #:obtain-value
   #:read-max-depth
   #:rebuild-rehashed-node
   #:rehash
   #:transactional-copy-on-write
   #:transactional-rebuild-rehashed-node
   #:with-destructive-erase-hamt
   #:with-hamt-path
   #:with-hash-tree-functions))
