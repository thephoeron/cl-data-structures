(in-package #:cl-data-structures.algorithms.meta)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function aggregator-finished-p
    (:description "Informs caller if aggregator finished aggregation and result can be obtained."
     :returns "T if aggregator finished and result can be obtained. NIL otherwise."))

  (function pass-to-aggregation
    (:description "Passes ELEMENT to the AGGREGATOR."
     :arguments (("AGGREGATOR" "Instance of aggregator.")
                 ("ELEMENT" "Element that is supposed to be aggregated."))
     :exceptional-situations "Will signal operation-not-allowed when aggregation cannot be performed."))

  (function expects-content-p
    (:description "Informs caller if aggregator expects elements passed on this stage.")))
