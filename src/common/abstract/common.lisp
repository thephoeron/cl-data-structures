(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (list t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initarg :ownership-tag)))


(defstruct tagged-node
  (lock (bt:make-lock))
  (ownership-tag nil :type list))


(declaim (inline acquire-ownership))
(-> acquire-ownership (tagged-node list) boolean)
(defun acquire-ownership (node ownership-tag)
  (bind (((:accessors (tag tagged-node-ownership-tag)
                      (lock tagged-node-lock))
          node))
    (or (eq tag ownership-tag)
        (when (null (car tag))
          (bt:with-lock-held (lock)
            (if (null (car tag))
                (progn (setf tag ownership-tag) t)
                (eq tag ownership-tag)))))))


(flet ((enclose (tag)
         (lambda ()
           (setf (car tag) nil))))
  (defun enclose-finalizer (obj)
    (trivial-garbage:finalize obj (enclose (read-ownership-tag obj)))))


(defmethod initialize-instance :after
    ((obj fundamental-ownership-tagged-object)
     &rest all &key &allow-other-keys)
  (declare (ignore all))
  (unless (slot-boundp obj '%ownership-tag)
    (setf (slot-value obj '%ownership-tag) (make-ownership-tag)))
  (enclose-finalizer obj))


(defmethod cl-ds:reset! ((obj fundamental-ownership-tagged-object))
  (bind (((:slots %ownership-tag) obj))
    (setf (car %ownership-tag) nil
          %ownership-tag (list t))
    (trivial-garbage:cancel-finalization obj)
    (enclose-finalizer obj)
    obj))
