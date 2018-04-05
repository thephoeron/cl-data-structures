(in-package #:cl-user)


(defpackage :cl-data-structures.algorithms
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:cl-ds #:bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.alg)
  (:export
   #:accumulate
   #:aggregate
   #:aggregation-finished-p
   #:aggregation-function
   #:apply-aggregation-function
   #:apply-layer
   #:apply-range-function
   #:chain
   #:gather-prior-states
   #:group-by
   #:group-by-function
   #:hash-join
   #:layer-function
   #:make-hash-table-range
   #:make-state
   #:multi-aggregation-function
   #:multi-aggregation-stages
   #:on-each
   #:proxy-box-range
   #:proxy-range
   #:range-function
   #:sequence-window
   #:state-result
   #:summary
   #:summary-function
   #:transformation!-function
   #:zip))


(defpackage :cl-data-structures.algorithms.meta
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:cl-ds #:bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.alg.meta)
  (:export
   #:aggregate
   #:aggregation-finished-p
   #:aggregation-function
   #:apply-aggregation-function
   #:apply-layer
   #:apply-range-function
   #:gather-prior-states
   #:layer-function
   #:make-state
   #:multi-aggregation-function
   #:multi-aggregation-stages
   #:range-function
   #:state-result
   #:transformation!-function))
