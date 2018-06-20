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
   #:apriori
   #:apriori-set
   #:association-frequency
   #:association-information-gain
   #:content
   #:find-association
   #:find-set
   #:make-apriori-set
   #:support
   #:total-entropy
   #:type-count))
