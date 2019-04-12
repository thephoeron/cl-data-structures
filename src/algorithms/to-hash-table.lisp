(in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-hash-table to-hash-table-function

  (:range &key key test size hash-table-key hash-table-value table)
  (:range &key
          (test 'eql)
          (key #'identity)
          (hash-table-key #'identity)
          (hash-table-value #'identity)
          (size 16)
          (table (make-hash-table :test test :size size)))

  (%table %hash-table-key %hash-table-value)

  ((&key size test hash-table-key hash-table-value table &allow-other-keys)
   (ensure-functionf hash-table-key hash-table-value)
   (check-type table hash-table)
   (setf %table table
         %hash-table-key hash-table-key
         %hash-table-value hash-table-value))
  ((element)
   (setf (gethash (funcall %hash-table-key element) %table)
         (funcall %hash-table-value element)))

  (%table))
