(cl:in-package #:cl-ds.dicts.hamt)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function make-functional-hamt-dictionary
    (:syntax "make-functional-hamt-dictionary hash-fn equal-fn &key max-depth => functional-hamt-dictionary"
     :arguments-and-values
     ((hash-fn "function that will be used to hash keys. Should return fixnum and be proper hashing function.")
      (equal-fn "function used to resolve conflicts."))
     :description
     "Constructs and return new functional-hamt-dictionary"

     :returns
     "new instance of functional-hamt-dictionary."

     :notes "In theory HAMT can use infinite length of hash but this implementation uses 60 oldest bits at most."))

  (function make-mutable-hamt-dictionary
    (:syntax "make-mutable-hamt-dictionary hash-fn equal-fn &key max-depth => mutable-hamt-dictionary"
     :arguments-and-values
     ((hash-fn "function that will be used to hash keys. Should return fixnum and be proper hashing function.")
      (equal-fn "function used to resolve conflicts."))
     :description
     "Constructs and returns a new mutable-hamt-dictionary"

     :returns
     "new instance of mutable-hamt-dictionary."

     :notes "In theory HAMT can use infinite length of hash but this implementation uses 60 oldest bits at most."))

  (type dictionary
    (:description "Container that provides location to value mapping. Either ordered or unordered."))

  (type hamt-dictionary
    (:description "Root HAMT dictionary class."))

  (type functional-hamt-dictionary
    (:description "HAMT dictionary that implements functional api."))

  (type mutable-hamt-dictionary
    (:description "HAMT dictionary that implements mutable api."))

  (type transactional-hamt-dictionary
    (:description "Transactional HAMT dictionary that implements mutable api.")))
