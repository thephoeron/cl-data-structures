(cl:in-package #:cl-data-structures.dicts.srrb)


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
                  :reader cl-ds:type-specialization
                  :initform t)))


(defmethod cl-ds.utils:cloning-information append
    ((vector fundamental-sparse-rrb-vector))
  '((:tree access-tree)
    (:tail access-tail)
    (:tail-mask access-tail-mask)
    (:shift access-shift)
    (:tree-size access-tree-size)
    (:tree-index-bound access-tree-index-bound)
    (:index-bound access-index-bound)
    (:element-type read-element-type)))


(defclass mutable-sparse-rrb-vector (cl-ds:mutable
                                     cl-ds.dicts:mutable-sparse-vector
                                     fundamental-sparse-rrb-vector)
  ())


(defclass functional-sparse-rrb-vector (cl-ds:functional
                                        cl-ds.dicts:functional-sparse-vector
                                        fundamental-sparse-rrb-vector)
  ())


(defclass transactional-sparse-rrb-vector
    (cl-ds:transactional
     cl-ds.dicts:transactional-sparse-vector
     cl-ds.common.abstract:fundamental-ownership-tagged-object
     mutable-sparse-rrb-vector)
  ()
  (:default-initargs :ownership-tag (cl-ds.common.abstract:make-ownership-tag)))
