(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (rests #() :type simple-vector)
  (content nil :type t))


(-> locate-node ((or null skip-list-node) t) simple-array)
(defun locate-node (skip-list-node item)
  cl-ds.utils:todo)
