(in-package #:cl-user)


(defpackage :cl-data-structures.counting
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.counting)
  (:export
   #:all-super-sets
   #:all-sets
   #:aposteriori-set
   #:set-index
   #:apriori-set
   #:association-frequency
   #:content
   #:find-association
   #:find-set
   #:make-association-set
   #:support
   #:type-count))
