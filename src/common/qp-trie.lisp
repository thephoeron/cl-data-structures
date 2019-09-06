(cl:in-package #:cl-user)


(defpackage :cl-data-structures.qp-trie
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.qp-trie))


(cl:in-package #:cl-ds.qp-trie)


(deftype half-byte ()
  '(unsigned-byte 4))


(deftype node-index ()
  '(integer 0 16))


(defstruct qp-trie-node
  (children-bitmask 0 :type (unsigned-byte 16))
  (store-bitmask 0 :type (unsigned-byte 16))
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


(defun qp-trie-node-get-or-insert-children! (node index)
  (declare (type qp-trie-node node)
           (type node-index index))
  (if (qp-trie-node-present-p node index)
      (qp-trie-node-ref node index)
      (qp-trie-node-insert-new-node! node index)))


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
                   (qp-trie-node-get-or-insert-children! half-byte-1)
                   (qp-trie-node-get-or-insert-children! half-byte-2)))
    (finally (let ((last-byte (aref bytes (the fixnum (1- length)))))
               (declare (type (unsigned-byte 8) last-byte))
               (~> node
                   (qp-trie-node-get-or-insert-children!
                    (ldb (byte 4 0) last-byte))
                   (qp-trie-node-mark-leaf!
                    (ldb (byte 4 4) last-byte))))
             (return qp-trie))))


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
      (leave (values node i)))
    (setf node (qp-trie-node-ref node half-byte-1))
    (unless (qp-trie-node-present-p node half-byte-2)
      (leave (values node i)))
    (setf node (qp-trie-node-ref node half-byte-2))
    (finally (let* ((last-byte (aref bytes (1- length)))
                    (half-byte-1 (ldb (byte 4 0) last-byte))
                    (half-byte-2 (ldb (byte 4 4) last-byte))
                    (result nil))
               (unless (qp-trie-node-present-p node half-byte-1)
                 (return (values node (1- length))))
               (setf node (qp-trie-node-ref node half-byte-1))
               (setf result (qp-trie-node-leaf-present-p node half-byte-2))
               (if result
                   (return (values node length))
                   (return (values node (1- length))))))))


(defun qp-trie-delete! (qp-trie bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (assert (not (emptyp bytes)))
  (bind ((length (length bytes))
         (last-byte-position (the fixnum (1- length)))
         ((:labels impl (node i))
          (declare (type qp-trie-node node)
                   (type fixnum i))
          (if (eql last-byte-position i)
              cl-ds.utils:todo
              cl-ds.utils:todo)))))
