(in-package #:cl-data-structures.algorithms.meta)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type range-function
    (:description "Fundamental class of all functions operating on ranges (layer and aggregator functions)."))

  (type aggregation-function
    (:description "Fundamental class of aggregation functions. Aggregation functions transform range into single result."
     :responsibilities ("Dispatches make-state."
                        "Dispatches state-result."
                        "Mutates state in aggregate function."
                        "Presents callable interface to the user.")
     :collaborators ("State" "Range" "Aggregator")
     :notes "Aggregation-function is part of moderatly complex user aggregation protocol."
     :see-also (make-state aggregate state-result)))

  (type linear-aggregator
    (:description "Simple, low level aggregator."
     :responsibilities ("Calls aggregate."
                        "Manages aggregation functions state.")
     :collaborators ("Aggregation-function" "Range")))

  (function make-linear-aggregator
    (:description "Construct LINEAR-AGGREGATOR."
     :arguments ((function "Instance of AGGREGATION-FUNCTION.")
                 (arguments "List of arguments passed to APPLY-AGGREGATION-FUNCTION.")
                 (key "Monoid function used akin to :KEY in CL:REDUCE."))))

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

  (function extract-result
    (:description "Extract final result of aggregation."
     :exceptional-situations "Will signal operation-not-allowed if aggregator is not finished."))

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

  (function aggregate
    (:description "Mutate function state."
     :affected-by "Class of FUNCTION."
     :arguments ((function "Instance of the AGGREGATE-FUNCTION class.")
                 (state "Object construced with the MAKE-STATE function.")
                 (element "Item fetched from a range."))
     :notes "Specialization for this generic function is required for each aggregation-function."))

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
