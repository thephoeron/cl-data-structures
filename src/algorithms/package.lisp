(in-package :cl-user)

(defpackage :cl-data-structures.algorithms.meta
  (:use c2cl cl-data-structures cl-data-structures.aux-package)
  (:nicknames cl-ds.alg.meta)
  (:export #:%key
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
           #:cleanup
           #:state-result
           #:transformation!-function))

(defpackage :cl-data-structures.algorithms
  (:use c2cl cl-data-structures.algorithms.meta cl-data-structures cl-data-structures.aux-package)
  (:nicknames cl-ds.alg)
  (:shadow #:extrema #:extremum)
  (:export #:%summary
           #:%summary
           #:*current-key*
           #:accumulate
           #:array-elementwise
           #:bidirectional-chain-of-ranges
           #:bidirectional-proxy-range
           #:cartesian
           #:chain
           #:first-element
           #:last-element
           #:connect
           #:connect-traversable
           #:count-elements
           #:count-elements-function
           #:count-elements-if
           #:count-elements-if-function
           #:cumulative-accumulate
           #:cumulative-accumulate-range
           #:distinct
           #:enumerate
           #:extrema
           #:extremum
           #:filtering-proxy
           #:flatten-lists
           #:forward-chain-of-ranges
           #:forward-multiplex-proxy
           #:forward-proxy-box-range
           #:forward-proxy-range
           #:frequency
           #:gather-prior-states
           #:group-by
           #:group-by-function
           #:group-by-result-range
           #:hash-table-range
           #:latch
           #:make-hash-table-range
           #:make-proxy
           #:multiplex
           #:on-each
           #:only
           #:partition-if
           #:proxy-box-range
           #:proxy-range
           #:random-access-chain-of-ranges
           #:random-access-proxy-range
           #:rate
           #:reservoir-sample
           #:with-reservoir-sample
           #:read-function
           #:read-key
           #:read-keys
           #:read-original-range
           #:repeat
           #:restrain-size
           #:should-skip
           #:shuffled-range
           #:split-into-chunks
           #:summary
           #:summary-result-range
           #:to-hash-table
           #:to-list
           #:to-vector
           #:translation
           #:transparent-to-chunking-mixin
           #:without
           #:wrap-chunk
           #:zip
           #:zip-traversable))
