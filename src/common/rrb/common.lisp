;; link to java implementation https://github.com/clojure/clojure/blob/0b73494c3c855e54b1da591eeb687f24f608f346/src/jvm/clojure/lang/PersistentVector.java
(in-package #:cl-data-structures.common.rrb)


(define-constant +bit-count+ 5)
(define-constant +maximum-children-count+ (ash 1 +bit-count+))
(define-constant +tail-mask+ (dpb 0 (byte +bit-count+ 0) most-positive-fixnum))

(deftype node-content ()
  `(simple-vector ,+maximum-children-count+))
(deftype node-size ()
  `(integer 0 ,+maximum-children-count+))


(defstruct (rrb-node (:include tagged-node))
  (size 0 :type node-size)
  (content (make-array +maximum-children-count+) :type node-content))


(-> rrb-node-deep-copy (rrb-node list) rrb-node)
(declaim (inline rrb-node-deep-copy))
(defun rrb-node-deep-copy (node ownership-tag)
  (make-rrb-node :ownership-tag ownership-tag
                 :content (copy-array (rrb-node-content node))))


(defun rrb-node-push! (node position element)
  (setf (aref (rrb-node-content node) position) element)
  node)


(defun rrb-node-push-into-copy (node position element ownership-tag)
  (let ((result-content (make-array +maximum-children-count+))
        (source-content (rrb-node-content node)))
    (setf (aref result-content position) element)
    (iterate
      (for i from 0 below position)
      (setf (aref result-content i) (aref source-content i)))
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop-in-the-copy (node position ownership-tag)
  (let ((result-content (make-array +maximum-children-count+))
        (source-content (rrb-node-content node)))
    (iterate
      (for i from 0 below position)
      (setf (aref result-content i) (aref source-content i)))
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop! (node position)
  (setf (aref (rrb-node-content node) position) nil))


(defclass rrb-container (fundamental-ownership-tagged-object)
  ((%root :accessor access-root
          :initarg :root
          :type (or null rrb-node)
          :documentation "root of the tree")
   (%shift :initarg :shift
           :accessor access-shift
           :type non-negative-fixnum
           :initform 0)
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size)
   (%tail :initform nil
          :type (or null rrb-node)
          :initarg :tail
          :accessor access-tail)))


(declaim (inline tail-offset))
(-> tail-offset (non-negative-fixnum) non-negative-fixnum)
(defun tail-offset (size)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (if (< size 32)
      0
      (~> size 1- (logand +tail-mask+))))
