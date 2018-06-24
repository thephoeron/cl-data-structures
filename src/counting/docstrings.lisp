(in-package #:cl-data-structures.counting)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function find-association
    (:description "Find and return APRIORI-SET object containing apriori set and aposteriori set. Returned object can be used to obtain association-frequency, support and to find super-sets."
     :arguments ((index "SET-INDEX instance.")
                 (apriori "List of content in the apriori set.")
                 (aposteriori "List of content the aposteriori set."))
     :exceptional-situations ("Will signal cl-ds:not-in-allowed-set if at least one element in either apriori or aposteriori can't be find in the index."
                              "Will return empty set if set does not exist in the index.")))

  (function apriori-set
    (:description "Returns SET-IN-INDEX object containing apriori part of association."
     :arguments ((set "Set containing association."))))

  (function aposteriori-set
    (:description "Returns SET-IN-INDEX object containing aposteriori part of association."
     :arguments ((set "Set containing association."))))

  (function association-frequency
    (:description "Returns frequency of association between aposteriori and apriori of association-set."
     :returns "Value between 0 and 1."))

  (function all-sets
    (:description "Obtain all sets up to size from the index."
     :arguments ((index "Index, containing all subsets.")
                 (maximal-size "Integer, only return sets up-to this size.")
                 (minimal-frequency "Real, only return sets with total frequency above this limit."))
     :exceptional-situations ("Will signal type-errors if minimal-frequency is not real or maximal-size is not of type integer."
                              "Will signal cl-ds:argument-out-of-bounds if minimal frequency is below 0 or above 1"
                              "Will signal cl-ds:argument-out-of-bounds if maximal-size is not above 0.")
     :returns "Forward range of SET-IN-INDEX objects.")))
