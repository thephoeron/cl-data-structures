(in-package #:cl-user)


(defpackage :cl-data-structures.dicts
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.dicts)
  (:export
   #:hash-content-tuple-hash
   #:content-tuple-location
   #:content-tuple-value
   #:bucket
   #:content-tuple
   #:dictionary
   #:find-content
   #:grow-bucket
   #:hash-content-tuple
   #:hashing-dictionary
   #:make-bucket
   #:read-equal-fn
   #:read-hash-fn
   #:shrink-bucket
   #:single-elementp))


(defpackage :cl-data-structures.dicts.hamt
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:cl-ds.utils
        #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds.dicts.hamt)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:functional-hamt-dictionary
   #:functional-hamt-dictionary-add
   #:functional-hamt-dictionary-erase
   #:functional-hamt-dictionary-insert
   #:functional-hamt-dictionary-update
   #:hamt-dictionary
   #:hamt-dictionary-at
   #:hamt-dictionary-size
   #:make-functional-hamt-dictionary
   #:make-mutable-hamt-dictionary
   #:mutable-hamt-dictionary
   #:mutable-hamt-dictionary-add!
   #:mutable-hamt-dictionary-erase!
   #:mutable-hamt-dictionary-insert!
   #:mutable-hamt-dictionary-update!
   #:read-max-depth
   #:transactional-hamt-dictionary
   #:transactional-hamt-dictionary-add!
   #:transactional-hamt-dictionary-erase!
   #:transactional-hamt-dictionary-insert!
   #:transactional-hamt-dictionary-update!))
