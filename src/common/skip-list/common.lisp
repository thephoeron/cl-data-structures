(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type t))


(cl-ds.utils:define-list-of-slots skip-list-node ()
  (pointers skip-list-node-rests)
  (content skip-list-node-content))


(-> locate-node (simple-vector t function) simple-array)
(defun locate-node (pointers item test)
  (iterate outer
    (declare (type fixnum i length last)
             (type simple-vector result))
    (with length = (length pointers))
    (with result = pointers)
    (with last = (1- length))
    (with i = last)
    (iterate
      (with node = (aref result i))
      (when (null node)
        (in outer (finish)))
      (unless (funcall test item (skip-list-node-content node))
        (decf i)
        (leave))
      (setf result (skip-list-node-pointers node)))
    (until (> 0 i))
    (finally (return-from outer result))))
