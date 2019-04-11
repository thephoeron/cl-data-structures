(in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-hash-table to-hash-table-function

  (:range &key key test size hash-table-key hash-table-value)
  (:range &key
          (test 'eql)
          (key #'identity)
          (hash-table-key #'identity)
          (hash-table-value #'identity)
          (size 16))

  (%table %hash-table-key %hash-table-value)

  ((&key size test hash-table-key hash-table-value &allow-other-keys)
   (ensure-functionf hash-table-key hash-table-value)
   (setf %table (make-hash-table :test test :size size)
         %hash-table-key hash-table-key
         %hash-table-value hash-table-value))
  ((element)
   (setf (gethash (funcall %hash-table-key element) %table)
         (funcall %hash-table-value element)))

  (%table))
