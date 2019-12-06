(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type t))


(cl-ds.utils:define-list-of-slots skip-list-node ()
  (pointers skip-list-node-rests)
  (content skip-list-node-content))


(-> locate-node (simple-vector t function) (or null skip-list-node))
(defun locate-node (pointers item test)
  (iterate
    (declare (type fixnum i)
             (type simple-vector result))
    (with result = pointers)
    (with i = (~> pointers length 1-))
    (for node = (aref result i))
    (if (and node
             (funcall test
                      (skip-list-node-content node)
                      item))
        (setf result (skip-list-node-pointers node)
              i (~> result length 1-))
        (decf i))
    (while (>= i 0))
    (finally (return node))))
