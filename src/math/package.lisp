(in-package #:cl-user)


(defpackage :cl-data-structures.math
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.math)
  (:export
   #:average
   #:bootstrap
   #:harmonic-average
   #:hodges-lehmann-estimator
   #:median-absolute-deviation
   #:moments
   #:mutual-information
   #:mutual-information-matrix
   #:harmonic-average-mutual-information
   #:simple-linear-regression
   #:standard-deviation
   #:statistical-summary
   #:variance))
