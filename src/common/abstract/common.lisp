(in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (box t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :initarg :ownership-tag)))


(defstruct tagged-node
  (lock (bt:make-lock))
  (ownership-tag nil :type t))


(declaim (inline acquire-ownership))
(-> acquire-ownership (tagged-node t) boolean)
(defun acquire-ownership (node ownership-tag)
  (bind (((:accessors (tag tagged-node-ownership-tag)
                      (lock tagged-node-lock))
          node))
    (or (eq tag ownership-tag)
        (when (null (unbox tag))
          (bt:with-lock-held (lock)
            (if (null (unbox tag))
                (progn (setf tag ownership-tag) t)
                (eq tag ownership-tag)))))))


(flet ((enclose (tag)
         (lambda ()
           (assert (unbox tag))
           (setf (unbox tag) nil))))
  (defun enclose-finalizer (obj)
    (let ((tag (read-ownership-tag obj)))
      (assert (unbox tag))
      (trivial-garbage:finalize obj (enclose tag)))))


(defmethod initialize-instance :after
    ((obj fundamental-ownership-tagged-object)
     &rest all &key &allow-other-keys)
  (declare (ignore all))
  (unless (slot-boundp obj '%ownership-tag)
    (setf (slot-value obj '%ownership-tag) (make-ownership-tag))))



(defgeneric reset-ownership-tag (object)
  (:method ((object fundamental-ownership-tagged-object))
    (bind (((:slots %ownership-tag) object))
      (setf (unbox %ownership-tag) nil
            %ownership-tag (make-ownership-tag)))))
