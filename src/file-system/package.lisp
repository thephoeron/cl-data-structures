(in-package :cl-user)


(defpackage :cl-data-structures.file-system
  (:use #:cl-data-structures.aux-package #:common-lisp)
  (:nicknames #:cl-ds.fs)
  (:shadow #:find)
  (:export
   #:line-by-line
   #:close-inner-stream
   #:with-file-ranges))
