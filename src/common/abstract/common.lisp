(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (box t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initform nil
                   :initarg :ownership-tag)))


(defstruct tagged-node
  (ownership-tag nil :type t))


(declaim (inline acquire-ownership))
(-> acquire-ownership ((or tagged-node list) t) boolean)
(defun acquire-ownership (node ownership-tag)
  (declare (optimize (speed 3)))
  (etypecase node
    (list (eq (cdr node) ownership-tag))
    (tagged-node (eq ownership-tag (tagged-node-ownership-tag node)))))

