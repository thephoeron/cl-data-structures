(in-package #:cl-user)


(defpackage :cl-data-structures.common.rrb
  (:use #:common-lisp
        #:cl-data-structures.common.abstract
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.rrb)
  (:export
   #:+bit-count+
   #:+depth+
   #:+maximum-children-count+
   #:+tail-mask+
   #:access-last-size
   #:access-lower-bound
   #:access-root
   #:access-shift
   #:access-size
   #:access-start
   #:access-tail
   #:access-tail-size
   #:sparse-rrb-node
   #:sparse-node
   #:with-sparse-rrb-node
   #:make-sparse-rrb-node
   #:sparse-rrb-node-contains
   #:sparse-nref
   #:sparse-rrb-node-contains
   #:sparse-rrb-node-content
   #:sparse-rrb-node-bitmask
   #:sparse-rrb-node-size
   #:deep-copy-sparse-rrb-node
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
   #:node-content
   #:nref
   #:read-element-type
   #:remove-tail
   #:rrb-at
   #:rrb-container
   #:rrb-node
   #:rrb-node-content
   #:rrb-node-push!
   #:rrb-node-push-into-copy
   #:rrb-range
   #:transactional-copy-on-write
   #:transactional-copy-on-write-without-tail))
