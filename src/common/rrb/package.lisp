(in-package #:cl-user)


(defpackage :cl-data-structures.common.rrb
  (:use #:common-lisp #:iterate #:serapeum #:alexandria #:metabang-bind
        #:cl-data-structures.common.abstract)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.common.rrb)
  (:export
   #:+bit-count+
   #:+depth+
   #:+maximum-children-count+
   #:access-root
   #:access-size
   #:access-tail
   #:access-tail-size
   #:copy-on-write
   #:copy-on-write-without-tail
   #:copy-on-write-without-tail
   #:destructive-write
   #:destructive-write-without-tail
   #:make-rrb-node
   #:make-node-content
   #:access-shift
   #:insert-tail
   #:remove-tail
   #:rrb-at
   #:rrb-container
   #:rrb-node-content
   #:transactional-copy-on-write
   #:transactional-copy-on-write-without-tail))
