(in-package #:cl-user)


(defpackage :cl-data-structures.math.auxilary
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in #:sum #:into)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.math.aux)
  (:export
   #:gamma))
