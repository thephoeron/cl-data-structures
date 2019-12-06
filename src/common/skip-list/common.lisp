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
    (declare (type fixnum i)
             (type simple-vector result))
    (with result = pointers)
    (with i = (~> pointers length 1-))
    (for node = (aref result i))
    (if (and node
             (funcall test item (skip-list-node-content node)))
        (setf result (skip-list-node-pointers node)
              i (~> result length 1-))
        (decf i))
    (until (> 0 i))
    (finally (return-from outer result))))
