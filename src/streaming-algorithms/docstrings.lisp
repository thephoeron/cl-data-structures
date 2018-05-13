(in-package #:cl-data-structures.streaming-algorithms)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function
   approximated-set-cardinality
   (:description "Calculates estimated set cardinality using HyperLogLog algorithm. This requires only a constant ammount of memory."
    :arguments ((range "Object to aggregate.")
                (bits "How many bits per register should be used?")
                (hash-fn "Hashing function. SXHASH will do for strings.")))))
