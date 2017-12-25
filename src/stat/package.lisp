(in-package #:cl-user)


(defpackage :cl-data-structures.statistics
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.stat)
  (:export
   #:average
   #:entropy))
