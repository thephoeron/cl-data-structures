(in-package #:cl-user)


(defpackage :cl-data-structures.utils
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds.utils)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:add-sinks
   #:add-into-queue
   #:bind-lambda
   #:cartesian
   #:cases
   #:cond+
   #:cond-compare
   #:copy-without
   #:distance
   #:distance-matrix
   #:each-in-matrix
   #:end-execution
   #:erase-from-vector
   #:extendable-vector
   #:fill-distance-matrix-from-vector
   #:import-all-package-symbols
   #:insert-or-replace
   #:lambda-list-to-bindings
   #:lazy-let
   #:lazy-shuffle
   #:let-generator
   #:lexicographic-compare
   #:lower-bound
   #:make-distance-matrix
   #:make-distance-matrix-from-vector
   #:make-pipe-fragment
   #:merge-ordered-vectors
   #:method-lambda-list-to-function-lambda-list
   #:mutate-matrix
   #:on-ordered-intersection
   #:optimize-value
   #:ordered-p
   #:parallel-fill-distance-matrix-from-vector
   #:parallel-make-distance-matrix-from-vector
   #:pipe-fragment
   #:pop-last
   #:read-size
   #:start-execution
   #:swap-if
   #:swapop
   #:todo
   #:try-find
   #:try-find-cell
   #:try-remove
   #:unfold-table
   #:with-vectors))
