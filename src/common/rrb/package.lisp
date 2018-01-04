(in-package #:cl-user)


(defpackage :cl-data-structures.common.rrb
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.rrb)
  (:export
   #:+depth+
   #:+bit-count+
   #:+maximum-children-count+
   #:access-root
   #:access-size))
