(in-package #:cl-data-structures.algorithms.meta)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type range-function
    (:description "Fundamental class of all functions operating on ranges (layer and aggregator functions)."))

  (type aggregation-function
    (:description "Fundamental class of aggregation functions. Aggregation functions transform range into single result."
     :notes "Aggregation-function is part of moderatly complex user aggregation protocol."
     :see-also (make-state aggregate state-result)))

  (type fundamental-aggregation-stage
    (:description "Fundamental class of aggregation stages."
     :notes "cl:function is acceptable stage as well."))

  (type linear-aggregator
    (:description "Simple, low level aggregator."))

  (function make-linear-aggregator
    (:description "Construct LINEAR-AGGREGATOR."
     :arguments ((function "Instance of AGGREGATION-FUNCTION.")
                 (arguments "List of arguments passed to APPLY-AGGREGATION-FUNCTION.")
                 (key "Monoid function used akin to :KEY in CL:REDUCE."))))

  (function aggregator-finished-p
    (:description "Predicate. Informs caller if aggregator finished aggregation and result can be obtained."
     :arguments (aggregator "Instance of FUNDAMENTAL-AGGREGATOR.")
     :affected-by "Class of AGGREGATOR."
     :returns "T if aggregator finished and result can be obtained. NIL otherwise."))

  (function pass-to-aggregation
    (:description "Passes ELEMENT to the AGGREGATOR."
     :arguments ((AGGREGATOR "Instance of aggregator.")
                 (ELEMENT "Element that is supposed to be aggregated."))
     :affected-by "Class of AGGREGATOR."
     :see-also (pass-to-aggregation-with-stage)
     :notes "MULTI-STAGE-AGGREGATOR will forward this logic to pass-to-aggregation-with-stage."
     :exceptional-situations "Will signal operation-not-allowed when aggregation cannot be performed (for instance because aggregator-finished-p returns T, which indicates finished aggregation)."))

  (function define-aggregation-function
    (:description "Defines all required methods and classes for aggregation functions."))

  (function multi-aggregation-stages
    (:description "Extracts aggregation stages of multi-aggregation-function."
     :returns "List of stages."
     :arguments ((function "Instance of multi-aggregation-function.")
                 (all "All key arguments passed to apply-aggergation-function"))))

  (function expects-content-p
    (:description "Informs caller if aggregator expects elements passed on this stage."
     :arguments ((aggregator "Instance of FUNDAMENTAL-AGGREGATOR"))
     :affected-by "Class of aggregator."
     :see-also (expects-content-with-stage-p)
     :notes "MULTI-STAGE-AGGREGATOR will forward this logic to expects-content-with-stage-p."))

  (function extract-result
    (:description "Extract final result of aggregation."
     :exceptional-situations "Will signal operation-not-allowed if aggregator is not finished."))

  (function extract-result-with-stage
    (:description "Extract result of stage."
     :see-also (extract-result)
     :notes "This function is called from extract-result on multistage-aggregator."))

  (function begin-aggregation
    (:description "Signal that you are about to pass content of range (once) to the aggregator. May be called multiple times for multistage in the case of multistage aggregators."
     :arguments ((aggregator "Instance of aggregator."))
     :exceptional-situations "May signal operation-not-allowed if aggregation cannot be started (because for instance aggregator already finished, or aggregation already has began)."))

  (function begin-aggregation-with-stage
    (:description "Signal that you are about to pass content of range (once) to the aggregation stage."
     :see-also (begin-aggregation)
     :arguments ((stage "Instance of aggregation-stage")
                 (aggregator "Instance of aggregator."))))

  (function end-aggregation-with-stage
    (:description "Signal that you finishing passing content of range (once) to the aggregation stage."
     :see-also (end-aggregation)
     :arguments ((stage "Instance of aggregation-stage")
                 (aggregator "Instance of aggregator."))))

  (function end-aggregation
    (:description "Signal that you finished passing content of range (once) to the aggregator. May be called multiple times for multistage in the case of multistage aggregators."
     :arguments ((aggregator "Instance of aggregator."))
     :see-also (end-aggregation-with-stage)
     :exceptional-situations "May signal operation-not-allowed if aggregation cannot be started (because for instance aggregator already finished)."))

  (function apply-layer
    (:description "Entry point to common layer function logic."
     :arguments ((range "Instance of FUNDAMENTAL-RANGE.")
                 (function "Instance of LAYER-FUNCTION."))
     :affected-by "Class of RANGE."))

  (function apply-aggregation-function
    (:description "Entry point to common aggregation function logic."
     :arguments ((range "Varies. Sometimes actually range, sometimes object of different class (for instance: aggregation-stage).")
                 (function "Instance of AGGREGATION-FUNCTION class.")
                 (key "Key used on each element to obtain object passed to aggregation.")
                 (all "Gathers all options required by make-state."))
     :see-also (apply-aggregation-function-with-aggregator)
     :affected-by "Class of range."
     :notes ("Always pass KEY."
             "This function is a little bit hairy. In multistage aggregation it is used to intercept function state of individiual aggregation stage.")
     :affected-by "Class of RANGE."))

  (function apply-aggregation-function-with-aggregator
    (:description "Perform aggregation with constructed aggregator."
     :arguments ((aggregator "Instance of FUNDAMENTAL-AGGREGATOR.")
                 (range "Instance of FUNDAMENTAL-RANGE.")
                 (function "Instance of AGGREGATION-FUNCTION class.")
                 (key "Key used on each element to obtain object passed to aggregation.")
                 (all "Gathers all options required by make-state."))
     :notes "Always pass KEY."
     :affected-by "Class of AGGREGATOR and class of RANGE."))

  (function expects-content-with-stage-p
    (:description "Informs caller if aggregation stage expects element passed."
     :arguments ((stage "Either instance of AGGREGATION-STAGE or CL:FUNCTION.")
                 (aggregator "Instance of MULTISTAGE-AGGREGATOR."))
     :affected-by "Class of STAGE, class of AGGREGATOR."
     :see-also (expects-content-p)
     :notes "This function is called from expects-content as it's implementation on multistage-aggregator."))

  (function aggregate
    (:description "Mutate function state."
     :affected-by "Class of FUNCTION."
     :arguments ((function "Instance of the AGGREGATE-FUNCTION class.")
                 (state "Object construced with the MAKE-STATE function.")
                 (element "Item fetched from a range."))
     :notes "Specialization for this generic function is required for each aggregation-function."))

  (function initialize-stage
    (:description "Called with all key arguments from apply-aggregation-function and stage to initialize stage."
     :arguments ((stage "fundamental-aggregation-stage instance")
                 (arguments "All key arguments passed to apply-aggregation-function."))))

  (function state-result
    (:description "Extracts result of aggregation function out of state argument."
     :notes "Specialization for this generic function is required for each aggregation-function."
     :affected-by "Class of function."
     :arguments ((function "Instance of aggregation-function.")
                 (state "Object returned by make-state function"))
     :see-also (aggregation-function aggregate make-state)))

  (function make-state
    (:description "Construct state of aggregation function. All key arguments passed to apply-aggregation-function are forwarded to make-state."
     :affected-by "Class of function."
     :notes ("There is no fundamental class for state. Anything can be used as it as long as it is mutable object. Dispatch of state releated functions is performed on the function instance."
             "Specialization for this generic function is required for each aggregation-function.")
     :see-also (aggregation-function aggregate state-result)
     :arguments ((aggregation-function "aggergation-function instance")
                 (all "All key arguments passed to apply-aggregation-function."))
     :returns "Aggregation state. It will be passed to aggregate function where it can be mutated, and to state-result where effective function result can be obtained.")))
