(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    gini-impurity gini-impurity-function

  (:range &key key test count-fn hash-table)

  (:range &key
          (key #'identity)
          (test 'eql)
          (hash-table (make-hash-table :test test))
          (count-fn (constantly 1)))

  (%table %total-count %count-fn)

  ((&key count-fn hash-table &allow-other-keys)
   (check-type hash-table hash-table)
   (setf %table hash-table
         %count-fn count-fn
         %total-count 0))

  ((element)
   (let ((count (funcall %count-fn element)))
     (incf %total-count count)
     (incf (gethash element %table 0) count)))

  ((iterate
     (for (class count) in-hashtable %table)
     (for prob = (/ count %total-count))
     (sum (* prob (- 1.0 prob))))))
