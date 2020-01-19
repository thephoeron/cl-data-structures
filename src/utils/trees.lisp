(cl:in-package #:cl-ds.utils)


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


(defun ancestor-p (all-ancestors-vector test-fn child ancestor)
  (let ((list-of-ancestors (find child all-ancestors-vector :test test-fn)))
    (when (null list-of-ancestors)
      (error 'program-error "No such node"))
    (null (eq (find ancestor
                    (rest list-of-ancestors)
                    :test test-fn)
              nil))))
