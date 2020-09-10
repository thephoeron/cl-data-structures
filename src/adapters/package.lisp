(cl:in-package #:cl-user)


(defpackage :cl-data-structures.adapters
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.adapters)
  (:export
   #:vector-range
   #:list-range
   #:offset-vector-range))
