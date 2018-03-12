(in-package #:cl-ds.dicts)

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type dictionary
    (:description "Container that provides location to value mapping. Either ordered or unordered."))

  (type hashing-dictionary
    (:description "Dictionary that uses hashing function. Hashing function is assumed to return fixnum."))

  (type functional-hashing-dictionary
    (:description "Functional variant of hashing a dictionary."))

  (type mutable-hashing-dictionary
    (:description "Mutable variant of hashing a dictionary."))

  (type transactional-hashing-dictionary
    (:description "Transactional variant of hashing a dictionary."))

  (type lazy-hashing-dictionary
    (:description "Lazy variant of a hashing dictionary."))

  (type mutable-dictionary
    (:description "Mutable variant of a dictionary."))

  (type transactional-dictionary
    (:description "Transactional variant of a dictionary."))

  (type functional-dictionary
    (:description "Functional variant of a dictionary."))

  (type transactional-dictionary
    (:description "Transactional variant of a dictionary."))

  (type lazy-dictionary
    (:description "Lazy variant of a dictionary."))

  (function find-content
    (:description "Attempts to find element under LOCATION in the the bucket."
     :notes "This function accepts other keys. In case of hashing dictionaries, one will be :hash that is expected to be fixnum."
     :arguments ((container "Container that owns bucket. Acts as passed interface for method dispatch.")
                 (bucket "Bucket that will be searched.")
                 (location "Location that will be searched."))
     :returns ("Element"
               "Boolean. T if element was found, NIL otherwise.")))

  (function single-element-p
    (:description "Checks if bucket holds just one element (if not, consider rehashing)."
     :arguments ((bucket "Bucket that will be checked."))
     :returns "Boolean. T if only single element exists in bucket, NIL otherwise."
     :notes "This function will return NIL if bucket is empty.")))
