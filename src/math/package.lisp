(in-package #:cl-user)


(defpackage :cl-data-structures.math
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.math)
  (:export
   #:average
   #:moments
   #:simple-linear-regression
   #:simple-linear-regression-with-error
   #:standard-deviation
   #:statistical-summary
   #:variance))
