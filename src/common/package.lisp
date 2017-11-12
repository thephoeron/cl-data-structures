(in-package #:cl-user)


(defpackage :cl-data-structures.common
  (:use #:common-lisp #:iterate #:serapeum #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common)
  (:export
   #:access-content
   #:eager-modification-operation-status
   #:empty-eager-modification-operation-status
   #:force-version
   #:lazy-box-container
   #:make-eager-modification-operation-status
   #:read-found
   #:read-value))

