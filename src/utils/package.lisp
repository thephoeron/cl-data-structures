(in-package #:cl-user)


(defpackage :cl-data-structures.utils
  (:use #:common-lisp #:iterate #:alexandria #:serapeum #:metabang-bind)
  (:nicknames #:cl-ds.utils)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:add-into-queue
   #:add-sinks
   #:bind-lambda
   #:cartesian
   #:cases
   #:cond+
   #:cond-compare
   #:copy-without
   #:mref
   #:half-matrix
   #:draw-sample-vector
   #:each-in-matrix
   #:end-execution
   #:erase-from-vector
   #:extendable-vector
   #:fill-distance-matrix-from-vector
   #:future-carousel
   #:if-else
   #:ordered-exclusion
   #:import-all-package-symbols
   #:insert-or-replace
   #:lambda-list-to-bindings
   #:lambda-list-to-call-form
   #:lazy-let
   #:lazy-shuffle
   #:let-generator
   #:lexicographic-compare
   #:lower-bound
   #:make-half-matrix
   #:make-distance-matrix-from-vector
   #:make-new-skip-vector
   #:add-to-list
   #:ordered-intersection
   #:sref
   #:skip-vector-without
   #:make-skip-vector
   #:make-future-carousel
   #:make-pipe-fragment
   #:merge-ordered-vectors
   #:method-lambda-list-to-function-lambda-list
   #:mutate-matrix
   #:on-ordered-intersection
   #:optimize-value
   #:ordered-p
   #:parallel-fill-distance-matrix-from-vector
   #:define-list-of-slots
   #:with-slots-for
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
