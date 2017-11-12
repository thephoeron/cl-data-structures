(in-package #:cl-ds.dicts)


(set-documentation
 'dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Container that provides location to value mapping. Either ordered or unordered.")


(set-documentation
 'hashing-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Dictionary that uses hashing function. Hashing function is assumed to return fixnum.")


(set-documentation
 'functional-hashing-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Functional variant of hashing a dictionary.")


(set-documentation
 'mutable-hashing-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Mutable variant of hashing a dictionary.")


(set-documentation
 'transactional-hashing-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Transactional variant of hashing a dictionary.")


(set-documentation
 'lazy-hashing-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Lazy variant of a hashing dictionary.")


(set-documentation
 'mutable-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Mutable variant of a dictionary.")


(set-documentation
 'transactional-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Transactional variant of a dictionary.")


(set-documentation
 'functional-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Functional variant of a dictionary.")


(set-documentation
 'transactional-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Transactional variant of a dictionary.")


(set-documentation
 'lazy-dictionary <mechanics> <class> cl-ds:*documentation*
 :description "Lazy variant of a dictionary.")


(set-documentation
 'find-content <mechanics> <generic> cl-ds:*documentation*
 :description "Attempts to find element under LOCATION in the the bucket."
 :notes "This function accepts other keys. In case of hashing dictionaries, one will be :hash that is expected to be fixnum."
 :arguments-and-values '((container "Container that owns bucket. Acts as passed interface for method dispatch.")
                         (bucket "Bucket that will be searched.")
                         (location "Location that will be searched."))
 :returns '("Element"
            "Boolean. T if element was found, NIL otherwise."))


(set-documentation
 'single-element-p <mechanics> <generic> cl-ds:*documentation*
 :description "Checks if bucket holds just one element (if not, consider rehashing)."
 :arguments-and-values '((bucket "Bucket that will be checked."))
 :returns "Boolean. T if only single element exists in bucket, NIL otherwise."
 :notes "This function will return NIL if bucket is empty.")
