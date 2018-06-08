(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (list t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initform nil
                   :initarg :ownership-tag)))


(defstruct tagged-node ownership-tag)


(declaim (inline acquire-ownership))
(-> acquire-ownership (t (or list tagged-node)) boolean)
(defun acquire-ownership (node ownership-tag)
  (declare (optimize (speed 3)))
  (etypecase node
    (list (eq (cdr node) ownership-tag))
    (tagged-node (eq (tagged-node-ownership-tag node) ownership-tag))))


(defun reset-ownership-tag (object)
  cl-ds.utils:todo)
