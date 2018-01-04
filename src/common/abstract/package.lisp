(in-package #:cl-user)


(defpackage :cl-data-structures.common.abstract
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.abstract)
  (:export
   #:access-root
   #:access-size))
