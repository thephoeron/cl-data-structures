(in-package :cl-user)

(defpackage :cl-data-structures.math
  (:nicknames cl-ds.math)
  (:use c2cl cl-data-structures.aux-package)
  (:shadow #:variance #:standard-deviation)
  (:export #:absolute-value-norm
           #:array-average
           #:array-geometric-average
           #:array-harmonic-average
           #:array-sum
           #:average
           #:beta0
           #:beta1
           #:bootstrap
           #:co-occurence-table
           #:entropy
           #:fast-map
           #:geometric-average
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
