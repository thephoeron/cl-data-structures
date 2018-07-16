(in-package #:cl-user)


(defpackage :cl-data-structures.clustering
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.cluster)
  (:export
   #:clara
   #:clara-variable-number-of-medoids))
