;; link to java implementation https://github.com/clojure/clojure/blob/0b73494c3c855e54b1da591eeb687f24f608f346/src/jvm/clojure/lang/PersistentVector.java
(in-package #:cl-data-structures.common.rrb)


(define-constant +bit-count+ 5)
(define-constant +maximum-children-count+ (ash 1 +bit-count+))
(deftype node-content ()
  `(simple-vector ,+maximum-children-count+))


(defstruct rrb-node
  (lock (bt:make-lock))
  ownership-tag
  (content (make-array +maximum-children-count+) :type node-content))


(defclass rrb-container ()
  ((%root :accessor access-root
          :initarg :root
          :documentation "root of the tree")
   (%ownership-tag :reader read-ownership-tag
                   :initarg :ownership-tag)
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size)))


(flet ((enclose (tag)
         (lambda ()
           (setf (car tag) nil))))
  (defun enclose-finalizer (obj)
    (trivial-garbage:finalize obj (enclose (read-ownership-tag obj)))))


(defmethod initialize-instance :after ((obj rrb-container)
                                       &rest all &key &allow-other-keys)
  (declare (ignore all))
  (unless (slot-boundp obj '%ownership-tag)
    (setf (slot-value obj '%ownership-tag) (list t)))
  (enclose-finalizer obj))
