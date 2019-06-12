(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    gini-impurity gini-impurity-function

  (:range &key key test count-fn hash-table)

  (:range &key
          (key #'identity)
          (test 'eql)
          (count-fn (constantly 1))
          (hash-table (make-hash-table :test test)))

  (%table %total-count %count-fn)

  ((&key count-fn hash-table &allow-other-keys)
   (ensure-functionf count-fn)
   (check-type hash-table hash-table)
   (setf %table hash-table
         %count-fn count-fn
         %total-count 0.0))

  ((element)
   (let ((count (funcall %count-fn element)))
     (incf %total-count count)
     (incf (gethash element %table 0.0) count)))

  ((iterate
     (for (class count) in-hashtable %table)
     (for prob = (/ count %total-count))
     (assert (<= prob 1))
     (sum (* prob (- 1.0 prob))))))
