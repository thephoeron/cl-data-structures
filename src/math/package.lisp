(in-package #:cl-user)


(defpackage :cl-data-structures.math
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.math)
  (:export
   #:average
   #:median-absolute-deviation
   #:moments
   #:simple-linear-regression
   #:standard-deviation
   #:statistical-summary
   #:variance))
