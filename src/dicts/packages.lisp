(in-package #:cl-user)


(defpackage :cl-data-structures.dicts
  (:use #:common-lisp #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds.dicts)
  (:export
   #:dictionary))


(defpackage :cl-data-structures.dicts.hamt
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:cl-ds.utils
        #:docstample #:docstample.mechanics)
  (:nicknames #:cl-ds.dicts.hamt)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export

   #:read-max-depth
   #:transactional-hamt-dictionary
   #:mutable-hamt-dictionary
   #:functional-hamt-dictionary
   #:hamt-dictionary

   #:make-functional-hamt-dictionary
   #:make-mutable-hamt-dictionary

   #:hamt-dictionary-at
   #:hamt-dictionary-size

   #:mutable-hamt-dictionary-insert!
   #:mutable-hamt-dictionary-update!
   #:mutable-hamt-dictionary-add!
   #:mutable-hamt-dictionary-erase!

   #:functional-hamt-dictionary-insert
   #:functional-hamt-dictionary-add
   #:functional-hamt-dictionary-erase
   #:functional-hamt-dictionary-update

   #:transactional-hamt-dictionary-insert!
   #:transactional-hamt-dictionary-update!
   #:transactional-hamt-dictionary-add!
   #:transactional-hamt-dictionary-erase!))
