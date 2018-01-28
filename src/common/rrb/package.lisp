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
   #:access-last-size
   #:access-lower-bound
   #:access-root
   #:access-shift
   #:access-size
   #:access-start
   #:access-tail
   #:access-tail-size
   #:access-upper-bound
   #:copy-on-write
   #:copy-on-write-without-tail
   #:copy-on-write-without-tail
   #:descend-into-tree
   #:destructive-write
   #:destructive-write-without-tail
   #:insert-tail
   #:make-node-content
   #:make-rrb-node
   #:remove-tail
   #:rrb-at
   #:rrb-container
   #:rrb-node-content
   #:rrb-node-push-into-copy
   #:rrb-node-range
   #:transactional-copy-on-write
   #:transactional-copy-on-write-without-tail))
