(in-package #:cl-user)


(defpackage :cl-data-structures.algorithms.meta
  (:use #:cl
        #:cl-data-structures
        #:cl-data-structures.aux-package
        #:cl-data-structures.utils)
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
  (:use #:cl
        #:cl-data-structures.algorithms.meta
        #:cl-data-structures
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.alg)
  (:export
   #:accumulate
   #:bidirectional-proxy-range
   #:cartesian
   #:chain
   #:count-elements
   #:count-elements-function
   #:count-elements-if
   #:count-elements-if-function
   #:distinct
   #:flatten-lists
   #:forward-proxy-range
   #:gather-prior-states
   #:group-by
   #:group-by-function
   #:hash-join
   #:latch
   #:make-hash-table-range
   #:make-proxy
   #:on-each
   #:only
   #:proxy-box-range
   #:proxy-range
   #:proxy-range-aggregator-outer-fn
   #:random-access-proxy-range
   #:read-original-range
   #:shuffled-range
   #:split-into-chunks
   #:to-vector
   #:transparent-to-chunking-mixin
   #:without
   #:wrap-chunk
   #:zip))
