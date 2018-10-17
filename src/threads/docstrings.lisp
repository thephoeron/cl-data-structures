(in-package #:cl-data-structures.threads)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function thread-buffer
    (:description "Creates proxy range that will present exactly the same context as original range. However, when calling TRAVERSE or ACROSS it will create internal thread with queue to values from inner thread, and pass it into queue. Main thread feeds data from the queue and calls passed callback on it. In result, it allows to speed up aggregation functions on nested complex pipes."
     :returns "BUFFER-RANGE subclass. Depending on the RANGE argument class it may be FORWARD-BUFFER-RANGE, BIDIRECTIONAL-BUFFER-RANGE or RANDOM-ACCESS-BUFFER-RANGE."))

  (function in-parallel
    (:description "Changes how traverse and accross functions work on inner ranges. Instead for working directly, it will first obtain chunked range, then each chunk will be scheduled to obtain values on lparallel worker. Results are then pushed into queue and read on the caller thread. Useful in conjuction with time consuming on-each functions.")))
