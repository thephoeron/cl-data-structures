(in-package #:cl-user)


(defpackage :cl-data-structures.counting
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in #:sum #:into)
  (:nicknames #:cl-ds.counting)
  (:export
   #:all-sets
   #:all-super-sets
   #:aposteriori-set
   #:apriori-set
   #:association-frequency
   #:content
   #:find-association
   #:find-set
   #:make-association-set
   #:set-index
   #:support
   #:type-count))
