(in-package #:cl-user)


(defpackage :cl-data-structures.adapters
  (:use #:common-lisp #:docstample #:docstample.mechanics #:alexandria
        #:iterate #:alexandria #:serapeum #:bind)
  (:nicknames #:cl-ds.adapters)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  )
