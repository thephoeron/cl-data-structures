(cl:in-package #:cl-data-structures.threads)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function thread-buffer
    (:description "Creates a proxy range that will present exactly the same content as the original range. However, when calling TRAVERSE or ACROSS an internal thread with queue will created. Values from the inner range will be read on the new thread and next passed to the queue. The main thread feeds data from the queue and calls passed callback on it."
     :exceptional-situations ("Will raise a TYPE-ERROR when CONTEXT-FUNCTION is not funcallable."
                              "Will raise a TYPE-ERROR when limit is not integer."
                              "Will raise CL-DS:ARGUMENT-OUT-OF-BOUNDS when limit is not at least 16.")
     :arguments-and-values ((range "The input range.")
                            (limit "The maximal size of the queue used internally. Setting this to a low value reduces memory overhead.")
                            (:context-function "Function that will accept and call another function. Defaults to #'FUNCALL. Can be used to establish a dynamic scope bindings when traversing the internal range."))
     :returns "Instance of BUFFER-RANGE subclass. Depending on the class of the RANGE it may be a FORWARD-BUFFER-RANGE, BIDIRECTIONAL-BUFFER-RANGE or RANDOM-ACCESS-BUFFER-RANGE."))

  (function in-parallel
    (:description "Changes how traverse and accross functions work on the inner ranges. Instead of working directly, it will first obtain a chunked range, then each chunk will be scheduled on a lparallel worker. Results are then pushed into queue and read on the caller thread. Useful in conjuction with time consuming on-each functions underneath."
     :notes ("This changes how aggregation functions work (because aggregation uses accross underneath)."
             "Calling this function on object that does not support CHUNKED function will result into fallback to ordinary traverse.")
     :exceptional-situations ("Will raise a TYPE-ERROR when CONTEXT-FUNCTION is not funcallable."
                              "Will raise a TYPE-ERROR when LIMIT or CHUNK-SIZE-HINT is not integer."
                              "Will raise CL-DS:ARGUMENT-OUT-OF-BOUNDS when either LIMIT or CHUNK-SIZE-HINT is not above 0.")
     :arguments-and-values ((range "Input range.")
                            (:limit "Maximal size of queue used internally. Setting this to low value reduces memory overhead.")
                            (:context-function "Function that should accept function and then call it. Can be used to establish a dynamic scope bindings for traversing the internal chunked range. Defaults to #'FUNCALL."))
     :returns "Instance of IN-PARALLEL-RANGE subclass.")))
