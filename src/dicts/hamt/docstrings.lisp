(in-package #:cl-ds.dicts.hamt)


(set-documentation
 'make-functional-hamt-dictionary <mechanics> <function> cl-ds:*documentation*
 :syntax "make-functional-hamt-dictionary hash-fn equal-fn &key max-depth => functional-hamt-dictionary"
 :arguments-and-values
 '((hash-fn "function that will be used to hash keys. Should return fixnum and be proper hashing function.")
   (equal-fn "function used to resolve conflicts.")
   (max-depth "how many level this HAMT can hold at most? Each level translates into additional 6 bits of hash used"))
 :description
 "Constructs and return new functional-hamt-dictionary"

 :returns
 "new instance of functional-hamt-dictionary."

 :notes "In theory HAMT can use infinite length of hash but this implementation uses 60 oldest bits at most."
 :exceptional-situations "Will raise condition of type ARGUMENT-OUT-OF-BOUNDS if max-depth is less than 1 or larger than 10.")


(set-documentation
 'make-mutable-hamt-dictionary <mechanics> <function> cl-ds:*documentation*
 :syntax "make-mutable-hamt-dictionary hash-fn equal-fn &key max-depth => mutable-hamt-dictionary"
 :arguments-and-values
 '((hash-fn "function that will be used to hash keys. Should return fixnum and be proper hashing function.")
   (equal-fn "function used to resolve conflicts.")
   (max-depth "how many level this HAMT can hold at most? Each level translates into additional 6 bits of hash used"))
 :description
 "Constructs and return new mutable-hamt-dictionary"

 :returns
 "new instance of mutable-hamt-dictionary."

 :notes "In theory HAMT can use infinite length of hash but this implementation uses 60 oldest bits at most."
 :exceptional-situations "Will raise condition of type ARGUMENT-OUT-OF-BOUNDS if max-depth is less than 1 or larger than 10.")


(set-documentation
 'dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Container that provides location to value mapping. Either ordered or unordered.")


