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
                (hash-fn "Hashing function. SXHASH will do for strings."))
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
                              510000)))])))
