(in-package #:cl-user)


(defpackage :cl-data-structures.adapters
  (:use #:common-lisp #:alexandria #:iterate
        #:alexandria #:serapeum #:bind)
  (:nicknames #:cl-ds.adapters)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:export
   #:vector-range
   #:offset-vector-range))
