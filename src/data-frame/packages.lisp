(in-package #:cl-user)


(defpackage cl-data-structures.data-frame
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in #:sum #:into)
  (:nicknames #:cl-ds.df)
  (:export
   #:*active-data*
   #:*active-frame*
   #:+column+
   #:+row+
   #:alias
   #:column
   #:column-at
   #:combine!
   #:data
   #:mutate!
   #:plane
   #:at-cell
   #:cell
   #:range-combine!
   #:range-stack
   #:data-frame
   #:fundamental-data-frame
   #:alias
   #:row
   #:row-at
   #:stack))
