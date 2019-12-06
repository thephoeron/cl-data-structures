(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type t))


(cl-ds.utils:define-list-of-slots skip-list-node ()
  (pointers skip-list-node-rests)
  (content skip-list-node-content))


(-> locate-node (simple-vector t function) (or null skip-list-node))
(defun locate-node (pointers item test)
  (let* ((pointers-length (~> pointers length))
         (last (1- pointers-length)))
    (iterate
      (declare (type fixnum i))
      (for i from last downto 0)
      (for node = (aref pointers i))
      (when (null node)
        (next-iteration))
      (for content = (skip-list-node-content node))
      (when (funcall test item content)
        (return-from locate-node node)))
    (iterate
      (declare (type fixnum i)
               (type simple-vector result))
      (with result = pointers)
      (with result-node = nil)
      (with i = last)
      (for node = (aref result i))
      (if (and node
               (funcall test
                        (skip-list-node-content node)
                        item))
          (setf result (skip-list-node-pointers node)
                i (~> result length 1-)
                result-node (aref result i))
          (decf i))
      (while (>= i 0))
      (finally (return result-node)))))


(defclass fundamental-skip-list ()
  ((%head :initarg :head
          :reader read-head)
   (%tail :initarg :tail
          :accessor access-tail)
   (%maximum-level :initarg :maximum-level
                   :accessor access-maximum-level)))


(cl-ds.utils:define-list-of-slots fundamental-skip-list ()
  (head read-head)
  (tail access-tail)
  (maximum-level access-maximum-level))


(defmethod cl-ds.utils:cloning-information append ((object fundamental-skip-list))
  '((:head read-head)
    (:tail access-tail)
    (:maximum-level access-maximum-level)))
