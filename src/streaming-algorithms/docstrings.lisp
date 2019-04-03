(in-package #:cl-data-structures.streaming-algorithms)
(eval-always
  (scribble:configure-scribble :package :cl-data-structures.streaming-algorithms)
  (named-readtables:in-readtable :scribble))

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
    approximated-set-cardinality
    (:description "Calculates the estimated set cardinality using the HyperLogLog algorithm. This requires only a constant (and modest) ammount of memory."
     :arguments ((range "Object to aggregate.")
                 (bits "How many bits per register should be used? Should be at least 4, and 20 at most. Large values are beneficial for high accuracy of the result but will require more memory.")
                 (hash-fn "Hashing function. SXHASH will do for strings.")
                 (key "A function used to extract extract value from each element."))
     :notes ("This algorithm gives a solid estimate for large sets, not so good for small sets."
             "Fairly sensitive to a hash function. Large avalanche factor is very helpful."
             "Can be used to (for instance) estimate number of keys before creating hash table. Good estimate of size minimizes rehashing and therefore reduces both memory allocation and time required to fill hash table.")
     :returns "Object storing internal state. Use CL-DS:VALUE to extract estimate from it."
     :examples [(let ((data (cl-ds:xpr (:i 0)
                              (when (< i 500000)
                                (cl-ds:send-recur (random 99999999999) :i (1+ i))))))
                  (prove:ok (< 490000
                               (cl-ds:value
                                (cl-data-structures.streaming-algorithms:approximated-set-cardinality
                                 data
                                 20
                                 #'sxhash))
                               510000)))]))

  (class
    fundamental-data-sketch
    (:description "The base class of all data sketches. Instances of this class can be passed to streaming algorihms as initial states, cloned and combined into unions."))

  (function
    approximated-counts
    (:description "Calculates estimated counts using Min-Count sketch alogrithm. This requiret only a constant ammount of memory."
     :arguments ((range "Object to aggregate.")
                 (hash-fn "Hashing function. SXHASH will do for strings.")
                 (space "Positive integer. Size of the counters array")
                 (count "Number of hashing functions used."))
     :returns "Object storing internal state. Use CL-DS:AT to extract count estimate for element from it. CL-DS:SIZE can be used to extract the total size of range that was aggregated."
     :notes ("Quality of the estimate directly depends on DEPTH and WIDTH."
             "Sensitive to a hash function. Large avalanche factor is very helpful.")))

  (function
    bloom-filter
    (:description "Creates bloom filter out of elements in the range. Bloom filter is memory efficient data structures allowing to check if item is absent from the range (if at returns nil, item is certainly absent, if at returns t item either present or not)."
     :returns "Bloom filter object. Use cl-ds:at to check if element is present. False positives are possible, false negatives are not possible."
     :arguments ((range "Input for the creation of the bloom filter.")
                 (space "Positive-fixnum. What is the bloom vector size?")
                 (count "How many bits are used for each item?")
                 (:key "Function used to extract value for to hashing.")
                 (:hashes "Optional hashes vector. Needs to be supplied in order to ensure that the same hash values are generated between different filters.")))))
