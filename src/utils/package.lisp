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
   #:distance
   #:distance-matrix
   #:each-in-matrix
   #:erase-from-vector
   #:extendable-vector
   #:insert-or-replace
   #:lazy-let
   #:make-distance-matrix
   #:import-all-package-symbols
   #:merge-ordered-vectors
   #:ordered-p
   #:pop-last
   #:swapop
   #:todo
   #:try-find
   #:try-find-cell
   #:try-remove
   #:with-vectors))
