(in-package #:cl-data-structures.data-frame)


(defun column-at (number)
  (rcurry #'column number))


(defun row-at (number)
  (rcurry #'row number))


(defun stack (dimension &rest data))


(defun range-stack (dimension range))
