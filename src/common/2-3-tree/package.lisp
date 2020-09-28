(in-package :cl-user)

(defpackage cl-data-structures.common.2-3-tree
  (:nicknames cl-ds.common.2-3)
  (:use c2cl
        cl-data-structures.aux-package)
  (:export #:access-root
           #:delete-back-from-tree
           #:delete-back-from-tree!
           #:insert-front-into-tree
           #:insert-front-into-tree!
           #:transactional-insert-front-into-tree!
           #:transactional-delete-back-from-tree!
           #:node
           #:2-node
           #:3-node
           #:access-left
           #:access-right
           #:access-middle
           #:tree))
