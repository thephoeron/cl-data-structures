(cl:in-package #:cl-user)


(defpackage :cl-data-structures.qp-trie
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.qp-trie))


(cl:in-package #:cl-ds.qp-trie)


(defstruct qp-trie-node
  (children-bitmask 0 :type (unsigned-byte 16))
  (store-bitmask 0 :type (unsigned-byte 16))
  (content #() :type simple-array))


(defclass qp-trie ()
  ((%root :initarg :root
          :accessor access-root))
  (:default-initargs :root (make-qp-trie-node)))


(defun qp-trie-node-index (node index)
  (logcount (ldb (byte index 0)
                 (qp-trie-node-children-bitmask node))))


(defun qp-trie-node-ref (node index)
  (aref (qp-trie-node-content node)
        (qp-trie-node-index node index)))


(defun qp-trie-node-size (node)
  (~> node
      qp-trie-node-children-bitmask
      logcount))


(defun qp-trie-node-insert-new-node (node index)
  (let* ((new-mask (~> node qp-trie-node-children-bitmask
                       (dpb 1 (byte 1 index) _)))
         (new-index (logcount (ldb (byte index 0) new-mask)))
         (old-content (qp-trie-node-content node))
         (new-size (logcount new-mask))
         (new-content (make-array new-size))
         (new-node (make-qb-tree-node :parent node)))
    (iterate
      (for i from 0 below new-index)
      (setf (aref new-content i) (aref old-content i)))
    (setf (aref new-content new-index) new-node)
    (iterate
      (for i from (1+ new-index) below new-size)
      (setf (aref new-content i) (aref old-content (1- i))))
    (setf (qp-trie-node-content node) new-content
          (qp-trie-node-children-bitmask node) new-mask)
    new-node))


(defun qp-trie-node-mark-leaf (node index)
  (let ((mask (qp-trie-node-store-bitmask node)))
    (setf (qp-trie-node-store-bitmask node)
          (dpb 1 (byte 1 index) mask))))


(defun qp-trie-node-present (node index)
  (~>> node
       qp-trie-node-children-bitmask
       (ldb-test (byte 1 index) _)))


(defun qp-trie-node-get-or-insert-children (node index)
  (if (qp-trie-node-present node index)
      (qp-trie-node-ref node index)
      (qp-trie-node-insert-new-node node index)))


(defun qp-trie-insert (qb-tree string)
  (when (emptyp string)
    (return-from qp-trie-insert qb-tree))
  (iterate
    (with bytes = (babel:string-to-octets string))
    (with node = (access-root qb-tree))
    (with length = (length bytes))
    (for i from 0 below (1- length))
    (for byte = (aref bytes i))
    (for half-byte-1 = (ldb (byte 4 0) byte))
    (for half-byte-2 = (ldb (byte 4 4) byte))
    (setf node (~> node
                   (qp-trie-node-get-or-insert-children half-byte-1)
                   (qp-trie-node-get-or-insert-children half-byte-2)))
    (finally (let ((last-byte (aref bytes (1- length))))
               (~> node
                   (qp-trie-node-get-or-insert-children
                    (ldb (byte 4 0) last-byte))
                   (qp-trie-node-mark-leaf
                    (ldb (byte 4 4) last-byte))))
             (return qb-tree))))


(defun qp-trie-node-leaf-present (node half-byte-2)
  (~>> node
       qp-trie-node-store-bitmask
       (ldb-test (byte 1 half-byte-2))))


(defun qp-trie-find (qb-tree string)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 0))
           (type simple-string string))
  (when (emptyp string)
    (return-from qp-trie-find nil))
  (iterate
    (declare (type (simple-array (unsigned-byte 8) (*))
                   bytes)
             (type fixnum i length))
    (with bytes = (babel:string-to-octets string))
    (with node = (access-root qb-tree))
    (with length = (length bytes))
    (for i from 0 below (1- length))
    (for byte = (aref bytes i))
    (for half-byte-1 = (ldb (byte 4 0) byte))
    (for half-byte-2 = (ldb (byte 4 4) byte))
    (unless (qp-trie-node-present node half-byte-1)
      (leave (values node i)))
    (setf node (qp-trie-node-ref node half-byte-1))
    (unless (qp-trie-node-present node half-byte-2)
      (leave (values node i)))
    (setf node (qp-trie-node-ref node half-byte-2))
    (finally (let* ((last-byte (aref bytes (1- length)))
                    (half-byte-1 (ldb (byte 4 0) last-byte))
                    (half-byte-2 (ldb (byte 4 4) last-byte))
                    (result nil))
               (unless (qp-trie-node-present node half-byte-1)
                 (return (values node (1- length))))
               (setf node (qp-trie-node-ref node half-byte-1))
               (setf result (qp-trie-node-leaf-present node half-byte-2))
               (if result
                   (return (values node length))
                   (return (values node (1- length))))))))
