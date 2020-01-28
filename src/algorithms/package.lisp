(cl:in-package #:cl-user)


(defpackage :cl-data-structures.algorithms.meta
  (:use #:cl
        #:cl-data-structures
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.alg.meta)
  (:export
   #:%key
   #:across-aggregate
   #:aggregator
   #:aggregation-function
   #:aggregator-constructor
   #:apply-aggregation-function
   #:apply-aggregation-function-with-aggregator
   #:apply-layer
   #:apply-range-function
   #:construct-aggregator
   #:define-aggregation-function
   #:extract-result
   #:layer-function
   #:let-aggregator
   #:linear-aggregator
   #:call-constructor
   #:pass-to-aggregation
   #:range-function
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
   #:read-function
   #:forward-proxy-box-range
   #:to-list
   #:accumulate
   #:bidirectional-chain-of-ranges
   #:forward-multiplex-proxy
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
   #:read-key
   #:group-by
   #:group-by-result-range
   #:summary-result-range
   #:%summary
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
