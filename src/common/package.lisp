(in-package #:cl-user)


(defpackage :cl-data-structures.common
  (:use #:common-lisp #:iterate #:serapeum #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common)
  (:export
   #:eager-modification-operation-status
   #:empty-eager-modification-operation-status
   #:make-eager-modification-operation-status
   #:read-found
   #:read-value))

