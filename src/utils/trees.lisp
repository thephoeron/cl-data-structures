(in-package #:cl-ds.utils)


(defun all-parents (root children-fn)
  (let ((result nil))
    (labels ((impl (parent stack)
               (push (list* parent stack) result)
               (let ((children (funcall children-fn parent)))
                 (if (emptyp children)
                     parent
                     (map 'list
                          (rcurry #'impl (cons parent stack))
                          children)))))
      (impl root nil)
      result)))


(defun as-cons-tree (root children-fn)
  (labels ((impl (parent)
             (let ((children (funcall children-fn parent)))
               (if (emptyp children)
                   parent
                   (list parent
                         (map 'list #'impl children))))))
    (impl root)))
