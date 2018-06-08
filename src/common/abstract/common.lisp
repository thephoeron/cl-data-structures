(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (make-hash-table :test 'eq))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initform nil
                   :initarg :ownership-tag)))


(defun register-ownership (ownership-tag node)
  (unless (null ownership-tag)
    (setf (gethash node ownership-tag) t)))


(declaim (inline acquire-ownership))
(-> acquire-ownership ((or tagged-node list) (or null hash-table)) boolean)
(defun acquire-ownership (node ownership-tag)
  (declare (optimize (speed 3)))
  (unless (null ownership-tag)
    (nth-value 1 (gethash node ownership-tag))))


(defun reset-ownership-tag (object)
  (let ((tag (read-ownership-tag object)))
    (unless (null tag)
      (clrhash tag))))
