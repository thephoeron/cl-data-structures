(in-package :cl-user)

(defpackage :cl-data-structures.adapters
  (:nicknames cl-ds.adapters clds.adapters clds/adapters clds-adapters)
  (:use c2cl cl-data-structures.aux-package)
  (:export #:vector-range
           #:list-range
           #:offset-vector-range))
