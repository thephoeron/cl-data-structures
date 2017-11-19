(in-package #:cl-data-structures.common)


(defclass whole-tree-range ()
  ((%stack :type list
           :accessor access-stack
           :initform nil)
   (%obtain-value  :type (-> ((-> () t) (-> (t) t)) t)
                   :reader read-obtain-value)))


(defmethod cl-ds:morep ((range whole-tree-range))
  (~> range access-stack null not))


(declaim (inline read-implementation))
(defun read-implementation (stack obtain-value)
  (if (null stack)
      (values stack nil nil)
      (flet ((push-to-stack (x)
               (push x stack))
             (pop-stack ()
               (pop stack)))
        (declare (dynamic-extent #'push-to-stack #'pop-stack))
        (let ((result (funcall obtain-value #'pop-stack #'push-to-stack)))
          (values stack result t)))))


(defmethod cl-ds:peek-front ((range whole-tree-range))
  (with-accessors ((stack access-stack)
                   (obtain-value read-obtain-value)) range
    (multiple-value-bind (new-stack result found)
        (read-implementation stack obtain-value)
      (declare (ignore new-stack))
      (values result found))))


(defmethod cl-ds:consume-front ((range whole-tree-range))
  (with-accessors ((stack access-stack)
                   (obtain-value read-obtain-value)) range
    (multiple-value-bind (new-stack result found)
        (read-implementation stack obtain-value)
      (setf stack new-stack)
      (values result found))))
