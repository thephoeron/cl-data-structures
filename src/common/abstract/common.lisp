(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (list t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :writer write-ownership-tag
                   :initform nil
                   :initarg :ownership-tag)))


(defclass tagged-node ()
  ((%ownership-tag :initarg :ownership-tag
                   :reader read-ownership-tag)))


(defun tagged-node-ownership-tag (node)
  (declare (optimize (speed 3) (safety 0)))
  (slot-value node '%ownership-tag))


(declaim (inline acquire-ownership))
(-> acquire-ownership (t (or list tagged-node t)) boolean)
(defun acquire-ownership (node ownership-tag)
  (declare (optimize (speed 3)))
  (typecase node
    (list (eq (cdr node) ownership-tag))
    (tagged-node (eq (tagged-node-ownership-tag node) ownership-tag))
    (t nil)))


(defun reset-ownership-tag (object)
  (write-ownership-tag (make-ownership-tag) object))


(defun replica (object isolate)
  (check-type object fundamental-ownership-tagged-object)
  (lret ((result (cl-ds:become-transactional object)))
    (when isolate
      (reset-ownership-tag object))))
