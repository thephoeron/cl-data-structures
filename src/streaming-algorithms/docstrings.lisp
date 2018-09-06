(in-package #:cl-data-structures.streaming-algorithms)
(eval-always
  (scribble:configure-scribble :package :cl-data-structures.streaming-algorithms)
  (named-readtables:in-readtable :scribble))

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
    approximated-set-cardinality
    (:description "Calculates estimated set cardinality using HyperLogLog algorithm. This requires only a constant ammount of memory."
     :arguments ((range "Object to aggregate.")
                 (bits "How many bits per register should be used? Should be at least 4, and 20 at most. Large values are prefered for accurate results.")
                 (hash-fn "Hashing function. SXHASH will do for strings.")
                 (key "Function used to extract extract value from each element."))
     :notes ("This algorithm gives solid estimates for large sets, not so good for small sets."
             "Fairly sensitive to a hash function. Large avalanche factor is very helpful."
             "Can be used to (for instance) estimate number of keys for hash table before creating one. Good estimate will minimize rehashing and reduce both memory that needs to allocated and time required to fill hash table.")
     :returns "Object storing internal state. Use CL-DS:VALUE to extract estimate from it."
     :examples [(let ((data (cl-ds:xpr (:i 0)
                              (when (< i 500000)
                                (send-recur (random 99999999999) :i (1+ i))))))
                  (prove:ok (< 490000
                               (cl-ds:value
                                (cl-data-structures.streaming-algorithms:approximated-set-cardinality
                                 data
                                 20
                                 #'sxhash))
                               510000)))]))

  (function
    approximated-counts
    (:description "Calculates estimated counts using Min-Count sketch alogrithm. This requiret only a constant ammount of memory."
     :arguments ((range "Object to aggregate.")
                 (hash-fn "Hashing function. SXHASH will do for strings.")
                 (depth "Positive integer. Along with WIDTH controls size of the internal counters table.")
                 (width "Positive integer. Along with DEPTH controls size of the internal counters table."))
     :returns "Object storing internal state. Use CL-DS:AT to extract count estimate for element from it. CL-DS:SIZE can be used to extract the total size of range that was aggregated."
     :notes ("Quality of the estimate directly depends on DEPTH and WIDTH."
             "Sensitive to a hash function. Large avalanche factor is very helpful."))))
