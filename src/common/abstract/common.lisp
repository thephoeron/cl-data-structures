(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (box t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initarg :ownership-tag)))


(declaim (inline acquire-ownership))
(-> acquire-ownership (list t) boolean)
(defun acquire-ownership (node ownership-tag)
  (eq (cdr node) ownership-tag))

