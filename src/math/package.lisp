(in-package #:cl-user)


(defpackage :cl-data-structures.math
  (:use #:common-lisp #:cl-data-structures.aux-package)
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
   #:co-occurence-table
   #:chi-squared
   #:standard-deviation
   #:statistical-summary
   #:variance))
