(in-package #:cl-data-structures.dicts.srrb)


(defclass fundamental-sparse-rrb-vector (cl-ds.dicts:fundamental-sparse-vector)
  ((%tree :initarg :tree
          :initform cl-ds.meta:null-bucket
          :accessor access-tree)
   (%tail :initarg :tail
          :accessor access-tail
          :initform nil)
   (%tail-mask :initarg :tail-mask
               :initform 0
               :accessor access-tail-mask)
   (%shift :initarg :shift
           :accessor access-shift
           :initform 0)
   (%tree-size :initarg :tree-size
               :accessor access-tree-size
               :initform 0)
   (%tree-index-bound :initarg :tree-index-bound
                      :initform 0
                      :accessor access-tree-index-bound)
   (%index-bound :initarg :index-bound
                 :accessor access-index-bound
                 :initform cl-ds.common.rrb:+maximum-children-count+)
   (%element-type :initarg :element-type
                  :reader read-element-type
                  :initform t)))


(defclass mutable-sparse-rrb-vector (cl-ds:mutable
                                     fundamental-sparse-rrb-vector)
  ())


(defclass functional-sparse-rrb-vector (cl-ds:functional
                                        fundamental-sparse-rrb-vector)
  ())


(defclass transactional-sparse-rrb-vector
    (cl-ds:transactional
     cl-ds.common.abstract:fundamental-ownership-tagged-object
     mutable-sparse-rrb-vector)
  ()
  (:default-initargs :ownership-tag (cl-ds.common.abstract:make-ownership-tag)))
