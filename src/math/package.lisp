(in-package #:cl-user)


(defpackage :cl-data-structures.math
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.math)
  (:export
   #:average
   #:beta0
   #:beta1
   #:bootstrap
   #:harmonic-average
   #:harmonic-average-mutual-information
   #:hodges-lehmann-estimator
   #:median-absolute-deviation
   #:moments
   #:mutual-information
   #:mutual-information-matrix
   #:optimal-split-point
   #:simple-linear-regression
   #:standard-deviation
   #:statistical-summary
   #:variance))
