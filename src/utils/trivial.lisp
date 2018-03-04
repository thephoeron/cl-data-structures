(in-package #:cl-data-structures.utils)


(define-symbol-macro todo
    (error 'cl-ds:not-implemented :text "Not implemented"))


(defun unfold-table (table)
  (make-array (reduce #'* (array-dimensions table))
              :element-type (array-element-type table)
              :displaced-to table))


(eval-always
  (defun method-lambda-list-to-function-lambda-list (lambda-list)
    (~>> lambda-list
         (mapcar (lambda (x) (if (listp x) (car x) x))))))


(eval-always
  (defun lambda-list-to-bindings (lambda-list)
    (~>> lambda-list
         (remove-if (rcurry #'member lambda-list-keywords))
         method-lambda-list-to-function-lambda-list)))
