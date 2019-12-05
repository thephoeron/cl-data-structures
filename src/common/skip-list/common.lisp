(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (rests #() :type simple-vector)
  (content nil :type t))
