(in-package :cl-user)


(defpackage :cl-data-structures.file-system
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.fs)
  (:export
   #:line-by-line
   #:with-file-ranges))
