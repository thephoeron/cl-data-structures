(in-package :cl-user)

(defpackage :cl-data-structures.common.qp-trie
  (:use c2cl cl-data-structures.aux-package)
  (:nicknames cl-ds.common.qp-trie)
  (:export #:qp-trie
           #:qp-trie-find
           #:map-qp-trie-nodes
           #:access-root
           #:make-qp-trie-node
           #:qp-trie-node-clone
           #:qp-trie-node
           #:qp-trie-dict-node
           #:qp-trie-node-ref
           #:qp-trie-node-leaf-present-p
           #:qp-trie-node-present-p
           #:make-qp-trie-node
           #:qp-trie-insert!
           #:half-byte-list-to-array
           #:qp-trie-delete!))

(in-package :cl-ds.common.qp-trie)

(deftype half-byte ()
  '(unsigned-byte 4))

(deftype node-mask ()
  '(unsigned-byte 16))

(deftype node-index ()
  '(integer 0 16))

(deftype full-mask ()
  '(unsigned-byte 32))

(cl-ds.common.abstract:define-tagged-untagged-node qp-trie-node
  (bitmask 0 :type full-mask)
  (content (make-array 0 :element-type 'qp-trie-node)
    :type '(simple-array qp-trie-node (*))))
