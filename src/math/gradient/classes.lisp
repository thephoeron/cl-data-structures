(in-package :cl-data-structures.math.gradient)


(defstruct tape-node
  (symbol nil :type symbol)
  (depends
   (make-array 0 :element-type 'fixnum
                 :initial-element 0)
   :type (simple-array fixnum (*)))
  (weights
   (make-array 0 :element-type 'double-float
                 :initial-element 0.0d0)
   :type (simple-array double-float (*))))


(defclass tape ()
  ((%nodes :initarg :nodes
           :reader read-nodes)
   (%variables :initarg :variables
               :reader read-variables)))
