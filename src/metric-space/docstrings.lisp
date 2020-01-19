(cl:in-package #:cl-data-structures.metric-space)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (class metric-space-dictionary
    (:description "Fundamental class for all metric-space-dictionaries"))

  (class mutable-metric-space-dictionary
    (:description "Fundamental class for all mutable metric space dictionaries"))

  (class metric-space-set
    (:description "Fundamental class for all metric space sets."))

  (class mutable-metric-space-set
    (:description "Fundamental class for all mutable metric space sets.")))
