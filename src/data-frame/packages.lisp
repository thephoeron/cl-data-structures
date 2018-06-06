(in-package #:cl-user)


(defpackage cl-data-structures.data-frame
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.df)
  (:export
   #:*active-data*
   #:*active-frame*
   #:column
   #:column-at
   #:stack
   #:range-stack
   #:combine!
   #:range-combine!
   #:alias
   #:data
   #:row
   #:row-at
   #:plane))
