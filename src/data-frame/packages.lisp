(in-package #:cl-user)


(defpackage cl-data-structures.data-frame
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.df)
  (:export
   #:*active-data*
   #:*active-frame*
   #:alias
   #:column
   #:column-at
   #:combine!
   #:data
   #:plane
   #:mutate!
   #:range-combine!
   #:range-stack
   #:row
   #:row-at
   #:stack))
