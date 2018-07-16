(in-package #:cl-user)


(defpackage :cl-data-structures.counting
  (:use #:common-lisp
        #:cl-data-structures.aux-package
        #:cl-data-structures.utils)
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
