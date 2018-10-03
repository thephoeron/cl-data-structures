(in-package #:cl-data-structures.dicts.srrb)


(defclass fundamental-sparse-rrb-vector
    (cl-ds.dicts:fundamental-sparse-vector)
  ((%tree :initarg :tree
          :initform nil
          :accessor access-tree)
   (%tail :initarg :tail
          :accessor access-tail
          :initform nil)
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
   (%tail-mask :initarg :tail-mask
               :initform 0
               :accessor access-tail-mask)
   (%element-type :initarg :element-type
                  :reader read-element-type
                  :initform t)))


(defclass mutable-sparse-rrb-vector (fundamental-sparse-rrb-vector
                                     cl-ds:mutable)
  ())


(defclass functional-sparse-rrb-vector (fundamental-sparse-rrb-vector
                                        cl-ds:functional)
  ())


(defclass transactional-sparse-rrb-vector
    (mutable-sparse-rrb-vector
     cl-ds.common.abstract:fundamental-ownership-tagged-object
     cl-ds:transactional)
  ())
