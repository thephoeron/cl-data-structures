(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type t))


(-> skip-list-node-level (skip-list-node) fixnum)
(defun skip-list-node-level (skip-list-node)
  (declare (optimize (speed 3)))
  (~> skip-list-node skip-list-node-pointers length))



(-> skip-list-node-at (skip-list-node cl-ds.utils:index) t)
(defun skip-list-node-at (skip-list-node index)
  (declare (optimize (speed 3)))
  (~> skip-list-node skip-list-node-pointers (aref index)))


(cl-ds.utils:define-list-of-slots skip-list-node ()
  (pointers skip-list-node-pointers)
  (level skip-list-node-level)
  (content skip-list-node-content))


(-> skip-list-node-clone (skip-list-node) skip-list-node)
(defun skip-list-node-clone (skip-list-node)
  (declare (optimize (speed 3)))
  (cl-ds.utils:with-slots-for (skip-list-node skip-list-node)
    (make-skip-list-node :pointers (copy-array pointers)
                         :content content)))


(-> copy-into (simple-vector simple-vector) simple-vector)
(defun copy-into (destination source)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (iterate
    (declare (type fixnum i))
    (for i from 0 below (length source))
    (setf (aref destination i) (aref source i))
    (finally (return destination))))


(declaim (notinline locate-node))
(-> locate-node (simple-vector t function) (values simple-vector
                                                   (or null simple-vector)))
(defun locate-node (pointers item test)
  (declare (optimize (speed 0) (safety 3) (debug 3)
                     (compilation-speed 0) (space 0)))
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
        (return-from locate-node (values pointers nil))))
    (iterate
      (declare (type fixnum i)
               (type simple-vector result))
      (with result = (copy-array pointers))
      (with prev-result = (make-array (length pointers)
                                      :initial-element nil))
      (with i = last)
      (for node = (aref result i))
      (cl-ds.utils:with-slots-for (node skip-list-node)
        (if (and node
                 (funcall test
                          content
                          item))
            (progn
              (copy-into prev-result result)
              (copy-into result pointers)
              (setf i (1- level)))
            (decf i)))
      (while (>= i 0))
      (finally
       (return (values result prev-result))))))


(defclass fundamental-skip-list ()
  ((%size :initarg :size
          :reader cl-ds:size
          :type fixnum
          :accessor access-size)
   (%pointers :initarg :pointers
             :reader read-pointers
             :type simple-vector)
   (%maximum-level :initarg :maximum-level
                   :accessor access-maximum-level))
  (:initial-values
   :size 0))


(cl-ds.utils:define-list-of-slots fundamental-skip-list ()
  (size access-size)
  (pointers read-pointers)
  (maximum-level access-maximum-level))


(defmethod cl-ds.utils:cloning-information append ((object fundamental-skip-list))
  '((:pointers read-pointers)
    (:size cl-ds:size)
    (:maximum-level access-maximum-level)))
