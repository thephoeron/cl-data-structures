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
     :exceptional-situations "Will signal operation-not-allowed when aggregation cannot be performed (for instance because aggregator-finished-p)."))

  (function expects-content-p
    (:description "Informs caller if aggregator expects elements passed on this stage."))

  (function extract-result
    (:description "Extract final result of aggregation."
     :exceptional-situations "Will signal operation-not-allowed if aggregator is not finished."))

  (function begin-aggregation
    (:description "Signal that you are about to pass content of range (once) to the aggregator. May be called multiple times for multistage in the case of multistage aggregators."
     :exceptional-situations "May signal operation-not-allowed if aggregation cannot be started (because for instance aggregator already finished, or aggregation already has began).")))
