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
    (:description "Returns frequency of association between aposteriori and apriori of association-set.")))
