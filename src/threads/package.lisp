(cl:in-package #:cl-user)


(defpackage :cl-data-structures.threads
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.threads)
  (:export
   ;; #:in-parallel
   #:thread-buffer
   #:parallel-multiplex
   ))
