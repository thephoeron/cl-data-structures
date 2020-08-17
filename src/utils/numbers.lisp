(cl:in-package #:cl-data-structures.utils)


(declaim (inline square))
(defun square (number)
  (* number number))
