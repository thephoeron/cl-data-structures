(in-package #:cl-data-structures.common)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function sequence-window
    (:description "Creates sequence window out of sequence or other sequence-window."
     :returns "Sequence-window object."
     :arguments ((sequence "Object with content.")
                 (from "Lower bound of resulting window.")
                 (to "Upper bound of resulting window.")))))
