(in-package #:cl-user)


(defpackage :cl-data-structures.dicts
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.dicts)
  (:export
   #:bucket
   #:content-tuple
   #:content-tuple-location
   #:content-tuple-value
   #:dictionary
   #:find-content
   #:functional-dictionary
   #:functional-hashing-dictionary
   #:hash-content-tuple
   #:hash-content-tuple-hash
   #:hashing-dictionary
   #:make-bucket
   #:mutable-dictionary
   #:mutable-hashing-dictionary
   #:read-equal-fn
   #:read-hash-fn
   #:single-element-p
   #:transactional-dictionary
   #:transactional-hashing-dictionary))


(defpackage :cl-data-structures.dicts.hamt
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:cl-ds.utils
        #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds.dicts.hamt)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:functional-hamt-dictionary
   #:hamt-dictionary
   #:hamt-dictionary-at
   #:hamt-dictionary-size
   #:make-functional-hamt-dictionary
   #:make-mutable-hamt-dictionary
   #:mutable-hamt-dictionary
   #:read-max-depth
   #:transactional-hamt-dictionary))
