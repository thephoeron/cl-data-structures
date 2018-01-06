;; link to java implementation https://github.com/clojure/clojure/blob/0b73494c3c855e54b1da591eeb687f24f608f346/src/jvm/clojure/lang/PersistentVector.java
(in-package #:cl-data-structures.common.rrb)


(define-constant +bit-count+ 5)
(define-constant +maximum-children-count+ (ash 1 +bit-count+))
(deftype node-content ()
  `(simple-vector ,+maximum-children-count+))


(defstruct (rrb-node (:include tagged-node))
  (content (make-array +maximum-children-count+) :type node-content))


(-> rrb-node-deep-copy (rrb-node list) rrb-node)
(declaim (inline rrb-node-deep-copy))
(defun rrb-node-deep-copy (node ownership-tag)
  (make-rrb-node :ownership-tag ownership-tag
                 :content (copy-array (rrb-node-content node))))


(defclass rrb-container (fundamental-ownership-tagged-object)
  ((%root :accessor access-root
          :initarg :root
          :documentation "root of the tree")
   (%shift :initarg :shift
           :accessor access-shift
           :type non-negative-fixnum
           :initform 0)
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size)))
