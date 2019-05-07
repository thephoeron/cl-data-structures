(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    enumerate enumerate-function

  (:range &key key test size hash-table-key table)
  (:range &key
          (test 'eql)
          (key #'identity)
          (hash-table-key #'identity)
          (size 16)
          (table (make-hash-table :test test :size size)))

  (%table %hash-table-key %number)

  ((&key size test hash-table-key table &allow-other-keys)
   (ensure-functionf hash-table-key)
   (check-type table hash-table)
   (setf %table table
         %hash-table-key hash-table-key))

  ((element)
   (let ((key (funcall %hash-table-key element)))
     (ensure (gethash key %table)
       (prog1 %number (incf %number)))))

  (%table))
