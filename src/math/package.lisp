(cl:in-package #:cl-user)


(defpackage :cl-data-structures.math
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.math)
  (:export
   #:absolute-value-norm
   #:array-sum
   #:average
   #:array-average
   #:array-harmonic-average
   #:beta0
   #:beta1
   #:bootstrap
   #:co-occurence-table
   #:entropy
   #:fast-map
   #:gini-impurity
   #:harmonic-average
   #:harmonic-average-mutual-information
   #:hidden-markov-model-generator
   #:hodges-lehmann-estimator
   #:median-absolute-deviation
   #:moments
   #:moving-average
   #:mutual-information
   #:mutual-information-matrix
   #:optimal-split-point
   #:simple-linear-regression
   #:standard-deviation
   #:sum
   #:variance))
