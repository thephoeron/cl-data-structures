(in-package #:cl-data-structures.utils)


(defmacro todo (&optional (description "Not implemented!"))
  `(error 'cl-ds:not-implemented
          :text ,description))


(defun unfold-table (table)
  (make-array (reduce #'* (array-dimensions table))
              :element-type (array-element-type table)
              :displaced-to table))
