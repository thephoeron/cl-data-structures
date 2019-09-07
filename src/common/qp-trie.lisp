(cl:in-package #:cl-user)


(defpackage :cl-data-structures.common.qp-trie
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.qp-trie)
  (:export #:qp-trie
           #:qp-trie-find
           #:access-root
           #:make-qp-trie-node
           #:qp-trie-insert!
           #:qp-trie-delete!))


(cl:in-package #:cl-ds.common.qp-trie)


(deftype half-byte ()
  '(unsigned-byte 4))


(deftype node-mask ()
  '(unsigned-byte 16))


(deftype node-index ()
  '(integer 0 16))


(defstruct qp-trie-node
  (children-bitmask 0 :type node-mask)
  (store-bitmask 0 :type node-mask)
  (content #() :type simple-array))


(defclass qp-trie ()
  ((%root :initarg :root
          :accessor access-root))
  (:default-initargs :root (make-qp-trie-node)))


(defun qp-trie-node-index (node index)
  (declare (type qp-trie-node node))
  (logcount (ldb (byte index 0)
                 (qp-trie-node-children-bitmask node))))


(defun qp-trie-node-ref (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (aref (qp-trie-node-content node)
        (qp-trie-node-index node index)))


(defun qp-trie-node-size (node)
  (declare (type qp-trie-node node))
  (~> node
      qp-trie-node-children-bitmask
      logcount))


(-> qp-trie-node-contains-leafs-p (qp-trie-node) boolean)
(defun qp-trie-node-contains-leafs-p (node)
  (~> node qp-trie-node-store-bitmask zerop not))


(-> qp-trie-node-delete! (qp-trie-node node-index) qp-trie-node)
(defun qp-trie-node-delete! (node index)
  (let* ((old-mask (qp-trie-node-children-bitmask node))
         (effective-index (logcount (ldb (byte index 0) old-mask)))
         (new-mask (dpb 0 (byte 1 index) old-mask))
         (old-content (qp-trie-node-content node))
         (old-size (logcount old-mask))
         (new-size (logcount new-mask))
         (new-content (make-array new-size)))
    (iterate
      (for i from 0 below effective-index)
      (setf (aref new-content i) (aref old-content i)))
    (iterate
      (for i from (1+ effective-index) below old-size)
      (setf (aref new-content (1- i)) (aref old-content i)))
    (setf (qp-trie-node-content node) new-content
          (qp-trie-node-children-bitmask node) new-mask)
    node))


(defun qp-trie-node-insert-new-node! (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (let* ((new-mask (~> node qp-trie-node-children-bitmask
                       (dpb 1 (byte 1 index) _)))
         (new-index (logcount (ldb (byte index 0) new-mask)))
         (old-content (qp-trie-node-content node))
         (new-size (logcount new-mask))
         (new-content (make-array new-size))
         (new-node (make-qp-trie-node)))
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


(-> qp-trie-node-unmark-leaf! (qp-trie-node node-index)
    (unsigned-byte 16))
(defun qp-trie-node-unmark-leaf! (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (let ((mask (qp-trie-node-store-bitmask node)))
    (setf (qp-trie-node-store-bitmask node)
          (dpb 0 (byte 1 index) mask))))


(defun qp-trie-node-mark-leaf! (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (let ((mask (qp-trie-node-store-bitmask node)))
    (setf (qp-trie-node-store-bitmask node)
          (dpb 1 (byte 1 index) mask))))


(defun qp-trie-node-present-p (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (~>> node
       qp-trie-node-children-bitmask
       (ldb-test (byte 1 index) _)))


(defun qp-trie-node-get-or-insert-child! (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (if (qp-trie-node-present-p node index)
      (qp-trie-node-ref node index)
      (qp-trie-node-insert-new-node! node index)))


(defun qp-trie-leaf-present-p (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (~>> node
       qp-trie-node-store-bitmask
       (ldb-test (byte 1 index))))


(defun qp-trie-insert! (qp-trie bytes)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type qp-trie qp-trie))
  (assert (not (emptyp bytes)))
  (iterate
    (declare (type fixnum length i half-byte-1 half-byte-2 byte)
             (type qp-trie-node node))
    (with node = (access-root qp-trie))
    (with length = (length bytes))
    (for i from 0 below (the fixnum (1- length)))
    (for byte = (aref bytes i))
    (for half-byte-1 = (ldb (byte 4 0) byte))
    (for half-byte-2 = (ldb (byte 4 4) byte))
    (setf node (~> node
                   (qp-trie-node-get-or-insert-child! half-byte-1)
                   (qp-trie-node-get-or-insert-child! half-byte-2)))
    (finally (let* ((last-byte (aref bytes (the fixnum (1- length))))
                    (next-node  (qp-trie-node-get-or-insert-child!
                                 node (ldb (byte 4 0) last-byte)))
                    (old-mask (qp-trie-node-store-bitmask next-node))
                    (new-mask (qp-trie-node-mark-leaf!
                               next-node
                               (ldb (byte 4 4) last-byte))))
               (declare (type (unsigned-byte 8) last-byte))
               (return (not (eql new-mask old-mask)))))))


(defun qp-trie-node-leaf-present-p (node half-byte)
  (declare (type qp-trie-node node)
           (type half-byte half-byte))
  (~>> node
       qp-trie-node-store-bitmask
       (ldb-test (byte 1 half-byte))))


(defun qp-trie-find (qp-trie bytes)
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) bytes))
  (assert (not (emptyp bytes)))
  (iterate
    (declare (type fixnum i length)
             (type half-byte half-byte-1 half-byte-2))
    (with node = (access-root qp-trie))
    (with length = (length bytes))
    (for i from 0 below (1- length))
    (for byte = (aref bytes i))
    (for half-byte-1 = (ldb (byte 4 0) byte))
    (for half-byte-2 = (ldb (byte 4 4) byte))
    (unless (qp-trie-node-present-p node half-byte-1)
      (leave i))
    (setf node (qp-trie-node-ref node half-byte-1))
    (unless (qp-trie-node-present-p node half-byte-2)
      (leave i))
    (setf node (qp-trie-node-ref node half-byte-2))
    (finally (let* ((last-byte (aref bytes (1- length)))
                    (half-byte-1 (ldb (byte 4 0) last-byte))
                    (half-byte-2 (ldb (byte 4 4) last-byte))
                    (result nil))
               (unless (qp-trie-node-present-p node half-byte-1)
                 (return (1- length)))
               (setf node (qp-trie-node-ref node half-byte-1))
               (setf result (qp-trie-node-leaf-present-p node half-byte-2))
               (if result
                   (return length)
                   (return (1- length)))))))


(defun qp-trie-delete! (qp-trie bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (assert (not (emptyp bytes)))
  (bind ((length (length bytes))
         (last-byte-position (the fixnum (1- length)))
         (deleted-p nil)
         ((:flet ref (node i))
          (if (qp-trie-node-present-p node i)
            (qp-trie-node-ref node i)
            (return-from qp-trie-delete! deleted-p)))
         ((:labels impl (node i))
          (declare (type qp-trie-node node)
                   (type fixnum i))
          (bind ((byte (aref bytes i))
                 (half-byte-1 (ldb (byte 4 0) byte))
                 (half-byte-2 (ldb (byte 4 4) byte))
                 (node (ref node half-byte-1)))
            (if (eql last-byte-position i)
                (let ((old-mask (qp-trie-node-store-bitmask node))
                      (new-mask (qp-trie-node-unmark-leaf! node
                                                           half-byte-2)))
                  (setf deleted-p (not (eql old-mask new-mask)))
                  (if (and (zerop new-mask)
                           (zerop (qp-trie-node-size node)))
                      nil
                      node))
                (let ((inner (impl (ref node half-byte-2)
                                   (1+ i))))
                  (if (and (null inner)
                           (eql 1 (qp-trie-node-size node)))
                      (if (~> node qp-trie-node-contains-leafs-p not)
                          nil
                          (qp-trie-node-delete! node half-byte-2))
                      (return-from qp-trie-delete! deleted-p)))))))
    (setf (access-root qp-trie)
          (or (impl (access-root qp-trie) 0)
              (make-qp-trie-node)))
    deleted-p))
