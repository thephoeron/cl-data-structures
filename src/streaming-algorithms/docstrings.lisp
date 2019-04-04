(in-package #:cl-data-structures.streaming-algorithms)
(eval-always
  (scribble:configure-scribble :package :cl-data-structures.streaming-algorithms)
  (named-readtables:in-readtable :scribble))

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function union
    (:description "Creates new data-sketch from the provided. Can be used to join sketches build on different data chunks."))

  (function approximated-set-cardinality
    (:description "Calculates the estimated set cardinality using the HyperLogLog algorithm. This requires only a constant (and modest) amount of memory."
     :arguments ((range "Object to aggregate.")
                 (:bits "How many bits per register should be used? Should be at least 4, and 20 at most. Large values are beneficial for high accuracy of the result but will require more memory.")
                 (:hash-fn "Hashing function. SXHASH will do for strings.")
                 (:data-sketch "Instead of the bits and the hash-fn, the user can pass a data-sketch argument.")
                 (:key "A function used to extract value from each element."))
     :notes ("This algorithm gives a solid estimate for large sets, not so good for small sets."
             "Fairly sensitive to a hash function. Large avalanche factor is very helpful."
             "Can be used to (for instance) estimate number of keys before creating a hash table. A good estimate of size minimizes rehashing and therefore reduces both memory allocation and time required to fill the hash table.")
     :returns "Instance of the fundamental-data-sketch class. Use CL-DS:VALUE to extract estimate from it."
     :examples [(let ((data (cl-ds:xpr (:i 0)
                              (when (< i 500000)
                                (cl-ds:send-recur (random 99999999999) :i (1+ i))))))
                  (prove:ok (< 490000
                               (cl-ds:value
                                (cl-data-structures.streaming-algorithms:approximated-set-cardinality
                                 data
                                 :bits 20
                                 :hash-fn #'sxhash))
                               510000)))]))

  (function clean-sketch
    (:description "Creates a new, empty data-sketch that would be produced by the function. New data-sketch can be cloned and passed as :data-sketch. This allows to keep compatibility between results of call to the streaming function."))

  (type fundamental-data-sketch
    (:description "The base class of all data sketches. Instances of this class can be passed to streaming algorihms as initial states, cloned and combined into unions."))

  (function approximated-counts
    (:description "Calculates estimated counts using Min-Count sketch algorithm. This requires only a constant amount of memory."
     :arguments ((range "Object to aggregate.")
                 (:hash-fn "Hashing function. SXHASH will do for strings.")
                 (:space "Positive integer. Size of the counters array")
                 (:count "Number of hashing functions used.")
                 (:data-sketch "Instead of the bits and the hash-fn, the user can pass a data-sketch argument."))
     :returns "Instance of the fundamental-data-sketch class. Use CL-DS:AT to extract count estimate for element from it."
     :notes ("Quality of the estimate directly depends on DEPTH and WIDTH."
             "Sensitive to a hash function. Large avalanche factor is very helpful.")))

  (function bloom-filter
    (:description "Creates bloom filter out of elements in the range. Bloom filter is memory efficient data structures allowing to check if an item is absent from the range (if AT returns nil, the item is certainly absent, if at returns T item either present or not)."
     :returns "Instance of the fundamental-data-sketch class. Use cl-ds:at to check if element is present. False positives are possible, false negatives are not possible."
     :arguments ((range "Input for the creation of the bloom filter.")
                 (:space "Positive-fixnum. What is the bloom vector size?")
                 (:count "How many bits are used for each item?")
                 (:key "Function used to extract value for to hashing.")
                 (:hashes "Optional hashes vector. Needs to be supplied in order to ensure that the same hash values are generated between different filters.")
                 (:data-sketch "Instead of the bits and the hash-fn, the user can pass a data-sketch argument.")))))
