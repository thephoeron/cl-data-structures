(in-package #:cl-user)


(defpackage :cl-data-structures.clustering
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.cluster)
  (:export
   #:clara
   #:clara-variable-number-of-medoids))
