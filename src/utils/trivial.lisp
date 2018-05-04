(in-package #:cl-data-structures.utils)


(define-symbol-macro todo
    (error 'cl-ds:not-implemented :text "Not implemented"))


(defun unfold-table (table)
  (make-array (reduce #'* (array-dimensions table))
              :element-type (array-element-type table)
              :displaced-to table))


(defun if-else (predicate true false)
  (ensure-function predicate)
  (ensure-function true)
  (ensure-function false)
  (lambda (&rest all)
    (if (apply predicate all)
        (apply true all)
        (apply false all))))
