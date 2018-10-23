(in-package #:cl-user)


(defpackage :cl-data-structures.common.rrb
  (:use #:common-lisp
        #:cl-data-structures.common.abstract
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.rrb)
  (:export
   #:+bit-count+
   #:+depth+
   #:+maximal-shift+
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
   #:access-upper-bound
   #:copy-on-write
   #:copy-on-write-without-tail
   #:copy-on-write-without-tail
   #:deep-copy-sparse-rrb-node
   #:descend-into-tree
   #:destructive-write
   #:destructive-write-without-tail
   #:insert-tail
   #:make-node-content
   #:make-rrb-node
   #:make-sparse-rrb-node
   #:node-content
   #:nref
   #:read-element-type
   #:remove-tail
   #:rrb-at
   #:rrb-container
   #:rrb-node
   #:rrb-node-content
   #:rrb-node-position
   #:rrb-node-push!
   #:rrb-node-push-into-copy
   #:rrb-range
   #:sparse-node
   #:sparse-nref
   #:sparse-rrb-mask
   #:sparse-rrb-node
   #:sparse-rrb-node-bitmask
   #:sparse-rrb-node-contains
   #:sparse-rrb-node-contains
   #:sparse-rrb-node-content
   #:sparse-rrb-node-erase
   #:sparse-rrb-node-erase!
   #:sparse-rrb-node-size
   #:transactional-copy-on-write
   #:transactional-copy-on-write-without-tail
   #:with-sparse-rrb-node-path
   #:with-sparse-rrb-node))
