(in-package :cl-user)


(defpackage :cl-data-structures.file-system
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.fs)
  (:shadow #:find)
  (:export
   #:close-inner-stream
   #:find
   #:line-by-line
   #:tokenize
   #:with-file-ranges))
