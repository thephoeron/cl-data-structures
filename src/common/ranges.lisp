(in-package #:cl-data-structures.common)


(defclass forward-tree-range ()
  ((%forward-stack :type list
                   :accessor access-forward-stack
                   :initform nil)
   (%obtain-value :type (-> ((-> () t) (-> (t) t)) (values t boolean))
                  :initarg :obtain-value
                  :reader read-obtain-value)
   (%obtain-value-reverse :type (–> ((-> () t) (-> (t) t)) (values t boolean))
                          :initarg :obtain-value-reverse
                          :reader read-obtain-value-reverse)))


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
                   (obtain-value read-obtain-value)) range
    (multiple-value-bind (new-stack result found)
        (read-implementation stack obtain-value)
      (declare (ignore new-stack))
      (values result found))))


(defmethod cl-ds:consume-front ((range forward-tree-range))
  (with-accessors ((stack access-forward-stack)
                   (obtain-value read-obtain-value)) range
    (multiple-value-bind (new-stack result found)
        (read-implementation stack obtain-value)
      (setf stack new-stack)
      (values result found))))


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
