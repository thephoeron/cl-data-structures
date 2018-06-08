(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (box t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initarg :ownership-tag)))


(defstruct tagged-node
  (ownership-tag nil :type t))


(declaim (inline acquire-ownership))
(-> acquire-ownership ((or tagged-node list) t) boolean)
(defun acquire-ownership (node ownership-tag)
  (if (listp node)
      (eq (cdr node) ownership-tag)
      (eq ownership-tag (tagged-node-ownership-tag node))))

