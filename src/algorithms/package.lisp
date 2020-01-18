(cl:in-package #:cl-user)


(defpackage :cl-data-structures.algorithms.meta
  (:use #:cl
        #:cl-data-structures
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.alg.meta)
  (:export
   #:%key
   #:abstract-proxy-aggregator
   #:across-aggregate
   #:aggregate
   #:aggregation-function
   #:apply-aggregation-function
   #:apply-aggregation-function-with-aggregator
   #:apply-layer
   #:apply-range-function
   #:construct-aggregator
   #:define-aggregation-function
   #:extract-result
   #:fundamental-aggregation-stage
   #:fundamental-aggregator
   #:layer-function
   #:linear-aggregator
   #:make-linear-aggregator
   #:make-state
   #:pass-to-aggregation
   #:range-function
   #:read-inner-aggregator
   #:read-key
   #:state-result
   #:transformation!-function))


(defpackage :cl-data-structures.algorithms
  (:use #:cl
        #:cl-data-structures.algorithms.meta
        #:cl-data-structures
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.alg)
  (:shadow #:extrema #:extremum)
  (:export
   #:%summary
   #:enumerate
   #:translation
   #:to-list
   #:accumulate
   #:bidirectional-chain-of-ranges
   #:multiplex
   #:bidirectional-proxy-range
   #:cartesian
   #:chain
   #:read-keys
   #:chain-traversable
   #:count-elements
   #:count-elements-function
   #:count-elements-if
   #:count-elements-if-function
   #:cumulative-accumulate
   #:cumulative-accumulate-range
   #:distinct
   #:to-hash-table
   #:extrema
   #:extremum
   #:filtering-proxy
   #:flatten-lists
   #:forward-chain-of-ranges
   #:forward-proxy-range
   #:gather-prior-states
   #:group-by
   #:group-by-result-range
   #:summary-result-range
   #:hash-table-range
   #:group-by-function
   #:hash-join
   #:latch
   #:make-hash-table-range
   #:make-proxy
   #:on-each
   #:only
   #:partition-if
   #:partition-if-with-callback
   #:proxy-box-range
   #:proxy-range
   #:proxy-range-aggregator-outer-fn
   #:random-access-chain-of-ranges
   #:random-access-proxy-range
   #:read-original-range
   #:repeat
   #:restrain-size
   #:should-skip
   #:shuffled-range
   #:split-into-chunks
   #:summary
   #:to-vector
   #:transparent-to-chunking-mixin
   #:without
   #:wrap-chunk
   #:zip
   #:connect
   #:connect-traversable
   #:rate
   #:frequency
   #:zip-traversable))
