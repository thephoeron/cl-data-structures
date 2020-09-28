(in-package :cl-user)

(defpackage cl-data-structures.common.egnat
  (:use c2cl cl-data-structures.aux-package)
  (:nicknames cl-ds.common.egnat)
  (:export #:access-root
           #:access-size
           #:distance
           #:fundamental-egnat-container
           #:get-value
           #:make-egnat-tree
           #:mutable-egnat-container
           #:prune-subtrees
           #:read-margin
           #:read-near
           #:select-children
           #:traverse-impl))
