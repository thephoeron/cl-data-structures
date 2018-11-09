(in-package #:cl-ds.utils)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type half-matrix
    (:description "Matrix container suitable for storing symetric data (like distances). Does not store diagonal values."))

  (function mref
    (:description "Matrix reference. Accessor for values in matrices."))

  (function if-else
    (:description "Construct function out of PREDICATE function, TRUE function and FALSE function. Checks if PREDICATE returns true, if yes: pass arguments to the TRUE function, if no: pass arguments to the FALSE function."
     :returns "Function."))

  (function lower-bound
    (:description "Find position in the VECTOR of the first element not larger then ELEMENT."))

  (function normalize-sequence-to-sum
    (:description "Normalizes SEQUENCE of numbers so sum of elements is equal to SUM."
     :returns "SEQUENCE"
     :side-effects "Mutates SEQUENCE."))

  (function normalize-sequence-to-span
    (:description "Normalizes SEQUENCE of numbers so every element is between MIN and MAX"
     :returns "SEQUENCE"
     :exceptional-situations "Assert: (< MIN MAX)"
     :side-effects "Mutates SEQUENCE."))

  (function remove-fill-pointer
    (:description "Accepts vector. Will return vector with the same content, but without fill-pointer."
     :exceptional-situations "TYPE-ERROR if VECTOR is not of type CL:VECTOR."
     :returns "VECTOR"))

  (function all-parents
    (:description "Scans tree with CHILDREN-FN (is supposed to return children of the PARENT as CL:SEQUENCE). Will return ALIST mapping node to list of all parents of nodes. Resulting data structure is useful as a way to lookup for partial order in the tree."))

  (function ancestor-p
    (:description "Checks if ANCESTOR is ancestor of CHILD in tree represented as ALL-ANCESTORS-VECTOR. ALL-ANCESTORS-VECTOR should be constructed by ALL-PARENTS function."))

  (function select-top
    (:description "Selects top elements from the vector, using heap select algorithm."
     :returns "Vector containing COUNT of top elements from the vector."
     :arguments-and-values ((vector "Input data.")
                            (count "Number of elements to select.")
                            (predicate "Function used to compare elements.")
                            (:key "Key function used to extract element for the predicate."))
     :side-effects "Will mutate orderd of elements in the VECTOR."))

  (function as-cons-tree
    (:description "Scans tree of arbitrary objects (CHILDREN-FN is supposed to return CL:SEQUENCE of children nodes) and returns it as a tree composed of cons cells.")))
