(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    enumerate enumerate-function

  (:range &key key test size)
  (:range &key
          (test 'eql)
          (key #'identity)
          (size 16))

  (%table %hash-table-key %number)

  ((setf %table (make-hash-table :test test :size size)))

  ((element)
   (let ((key (funcall %hash-table-key element)))
     (ensure (gethash key %table)
       (prog1 %number (incf %number)))))

  (%table))
