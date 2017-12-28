(in-package #:cl-user)


(defpackage :cl-data-structures.utils
  (:use #:common-lisp #:iterate #:alexandria #:serapeum)
  (:nicknames #:cl-ds.utils)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:bind-lambda
   #:cartesian
   #:cases
   #:cond+
   #:cond-compare
   #:copy-without
   #:distance
   #:distance-matrix
   #:each-in-matrix
   #:erase-from-vector
   #:extendable-vector
   #:import-all-package-symbols
   #:insert-or-replace
   #:lazy-let
   #:lazy-shuffle
   #:let-generator
   #:lower-bound
   #:make-distance-matrix
   #:make-distance-matrix-from-vector
   #:merge-ordered-vectors
   #:mutate-matrix
   #:on-ordered-intersection
   #:ordered-p
   #:parallel-make-distance-matrix-from-vector
   #:pop-last
   #:read-size
   #:swap-if
   #:swapop
   #:todo
   #:try-find
   #:try-find-cell
   #:try-remove
   #:with-vectors))
