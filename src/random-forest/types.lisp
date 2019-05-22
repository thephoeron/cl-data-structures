(cl:in-package #:cl-ds.rf)


(defclass fundamental-random-forest ()
  ())


(defclass random-forest-classifier (fundamental-random-forest)
  ((%tree-count :initarg :tree-count
                :reader tree-count)
   (%tree-maximum-depth :initarg :tree-maximum-depth
                        :reader tree-maximum-depth)
   (%tree-minimal-size :initarg :tree-minimal-size
                       :reader tree-minimal-size)
   (%split-attempts :initarg :split-attempts
                    :reader split-attempts)
   (%submodels :initarg :submodels
               :reader submodels
               :accessor access-submodels)
   (%submodel-class :initarg :submodel-class
                    :reader read-submodel-class)
   (%submodel-arguments :initarg :submodel-arguments
                        :reader submodel-arguments))
  (:default-initargs
   :submodel-arguments '()
   :tree-maximum-depth 2
   :split-attempts 10
   :submodels nil
   :tree-count 50))


(defclass tree-node ()
  ((%class :initarg :class
           :type (or null fixnum)
           :accessor access-class))
  (:default-initargs
   :class nil))


(defclass subtree-node (tree-node)
  ((%children :initarg :children
              :reader read-children))
  (:default-initargs
   :children '()
   :class nil))

(defclass leaf-node (tree-node)
  ())
