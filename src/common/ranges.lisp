(in-package #:cl-data-structures.common)


(defclass forward-tree-range (cl-ds:fundamental-forward-range)
  ((%forward-stack :type list
                   :accessor access-forward-stack
                   :initarg :forward-stack
                   :initform nil)
   (%obtain-value :type (-> ((-> () t) (-> (t) t)) (values t boolean))
                  :initarg :obtain-value
                  :reader read-obtain-value)
   (%key :type (-> (t) t)
         :initarg :key
         :reader read-key)))


(defmethod cl-ds:morep ((range forward-tree-range))
  (~> range access-forward-stack null not))


(declaim (inline read-implementation))
(defun read-implementation (stack obtain-value)
  (if (null stack)
      (values stack nil nil)
      (flet ((push-to-stack (x)
               (push x stack))
             (pop-stack ()
               (when (endp stack)
                 (return-from read-implementation
                   (values stack nil nil)))
               (pop stack)))
        (declare (dynamic-extent #'push-to-stack #'pop-stack))
        (let ((result (funcall obtain-value #'pop-stack #'push-to-stack)))
          (values stack result t)))))


(defmethod cl-ds:peek-front ((range forward-tree-range))
  (with-accessors ((stack access-forward-stack)
                   (obtain-value read-obtain-value)
                   (key read-key)) range
    (multiple-value-bind (new-stack result found)
        (read-implementation stack obtain-value)
      (declare (ignore new-stack))
      (values (when found (funcall key result)) found))))


(defmethod cl-ds:consume-front ((range forward-tree-range))
  (with-accessors ((stack access-forward-stack)
                   (obtain-value read-obtain-value)
                   (key read-key)) range
    (multiple-value-bind (new-stack result found)
        (read-implementation stack obtain-value)
      (setf stack new-stack)
      (values (when found (funcall key result)) found))))


(defmethod cl-ds:clone ((range forward-tree-range))
  (make 'forward-tree-range
        :forward-stack (mapcar #'cl-ds:clone (access-forward-stack range))
        :obtain-value (read-obtain-value range)
        :key (read-key range)))


;; (defmethod cl-ds:consume-back ((range forward-tree-range))
;;   (with-accessors ((stack access-backward-stack)
;;                    (obtain-value read-obtain-value-reverse)) range
;;     (multiple-value-bind (new-stack result found)
;;         (read-implementation stack obtain-value)
;;       (setf stack new-stack)
;;       (values result found))))


;; (defmethod cl-ds:peek-back ((range forward-tree-range))
;;   (with-accessors ((stack access-backward-stack)
;;                    (obtain-value read-obtain-value-reverse)) range
;;     (multiple-value-bind (new-stack result found)
;;         (read-implementation stack obtain-value)
;;       (declare (ignore new-stack))
;;       (values result found))))
