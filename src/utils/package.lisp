(in-package #:cl-user)


(defpackage :cl-data-structures.utils
  (:use #:common-lisp #:iterate #:alexandria #:serapeum)
  (:nicknames #:cl-ds.utils)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:bind-lambda
   #:cases
   #:cond+
   #:cond-compare
   #:copy-without
   #:erase-from-vector
   #:extendable-vector
   #:insert-or-replace
   #:lazy-let
   #:merge-ordered-vectors
   #:ordered-p
   #:pop-last
   #:swapop
   #:todo
   #:try-find
   #:try-find-cell
   #:try-remove
   #:with-vectors))
