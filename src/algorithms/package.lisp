(in-package #:cl-user)


(defpackage :cl-data-structures.algorithms
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate #:cl-ds)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.alg)
  (:export
   #:accumulate
   #:aggregate
   #:aggregation-function
   #:apply-aggregation-function
   #:apply-layer
   #:apply-range-function
   #:change-each!
   #:group-by
   #:group-by-function
   #:layer-function
   #:make-state
   #:on-each
   #:proxy-box-range
   #:proxy-range
   #:range-function
   #:state-result
   #:transformation!-function))
