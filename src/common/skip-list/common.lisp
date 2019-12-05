(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (rests #() :type simple-vector)
  (content nil :type t))


(cl-ds.utils:define-list-of-slots skip-list-node ()
  (rests skip-list-node-rests)
  (content skip-list-node-content))


(-> locate-node (simple-vector t function) simple-array)
(defun locate-node (pointers item test)
  (iterate
    (declare (type fixnum i length))
    (with length = (length pointers))
    (with result = (copy-array pointers))
    (for i from (the fixnum (1- length)) downto 0)
    (iterate
      )
    (finally (return result))))
