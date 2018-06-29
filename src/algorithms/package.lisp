(in-package #:cl-user)


(defpackage :cl-data-structures.algorithms.meta
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:cl-ds #:bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.alg.meta)
  (:export
   #:%key
   #:aggregate
   #:aggregation-function
   #:aggregation-stage
   #:aggregation-stage
   #:aggregator-finished-p
   #:apply-aggregation-function
   #:apply-aggregation-function-with-aggregator
   #:apply-layer
   #:define-aggregation-function
   #:apply-range-function
   #:begin-aggregation
   #:construct-aggregator
   #:end-aggregation
   #:expects-content-p
   #:expects-content-with-stage-p
   #:extract-result
   #:fundamental-aggregation-stage
   #:fundamental-aggregator
   #:gather-prior-states
   #:layer-function
   #:linear-aggregator
   #:make-linear-aggregator
   #:make-multi-stage-linear-aggregator
   #:make-state
   #:multi-aggregation-function
   #:multi-aggregation-stages
   #:multi-aggregator
   #:pass-to-aggregation
   #:range-function
   #:read-key
   #:reduce-stage
   #:stage
   #:state-result
   #:transformation!-function))

(defpackage :cl-data-structures.algorithms
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:cl-ds #:bind #:cl-ds.alg.meta)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.alg)
  (:export
   #:accumulate
   #:chain
   #:count-elements
   #:count-elements-function
   #:count-elements-if
   #:count-elements-if-function
   #:gather-prior-states
   #:group-by
   #:group-by-function
   #:hash-join
   #:make-hash-table-range
   #:make-proxy
   #:on-each
   #:only
   #:shuffled-range
   #:proxy-box-range
   #:proxy-range
   #:proxy-range-aggregator-outer-fn
   #:split-into-chunks
   #:to-vector
   #:without
   #:without-function
   #:flatten-lists
   #:zip))
