(in-package #:cl-user)


(defpackage :cl-data-structures.clustering
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.cluster)
  (:import-from cl-ds.utils.cluster silhouette cluster-contents)
  (:export
   #:clara
   #:silhouette
   #:cluster-contents
   #:clara-variable-number-of-medoids))
