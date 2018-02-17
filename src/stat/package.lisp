(in-package #:cl-user)


(defpackage :cl-data-structures.statistics
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:shadow #:variance #:standard-deviation)
  (:nicknames #:cl-ds.stat)
  (:export
   #:average
   #:variance
   #:simple-linear-regression
   #:standard-deviation))
