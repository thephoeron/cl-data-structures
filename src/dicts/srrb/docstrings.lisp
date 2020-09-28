(cl:in-package #:cl-ds.dicts.srrb)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type mutable-sparse-rrb-vector
    (:description "SRRB sparse vector that implements mutable api."))

  (type functional-sparse-rrb-vector
    (:description "SRRB sparse vector that implements mutable api."))

  (type transactional-sparse-rrb-vector
        (:description "Transactional SRRB sparse vector that implements mutable api."))

  (function make-functional-sparse-rrb-vector
            (:description "Constructs and returns a functional variant of the sparse rrb vector."))

  (function make-mutable-sparse-rrb-vector
            (:description "Constructs and returns a mutable variant of the sparse rrb vector."))

  (function make-transactional-sparse-rrb-vector
            (:description "Constructs and returns a transactional variant of the sparse rrb vector."))
  )
