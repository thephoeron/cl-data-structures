(in-package #:cl-user)


(defpackage :cl-data-structures.math.gradient
  (:use #:common-lisp #:serapeum #:cl-ds.utils
        #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.math.grad)
  (:export
   #:compile-gradient-expression
   #:define-tape-backward
   #:gradient
   #:gradient-expression
   #:tape-backward
   #:tape-backward-form
   #:tape-forward))
