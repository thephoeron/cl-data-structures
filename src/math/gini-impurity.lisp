(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    gini-impurity gini-impurity-function

  (:range &key key test)

  (:range &key (key #'identity) (test 'eql))

  (%table %total-count)

  ((&key test)
   (setf %table (make-hash-table :test test)
         %total-count 0))

  ((element)
   (incf %total-count)
   (incf (gethash element %table 0)))

  ((iterate
     (for (class count) in-hashtable %table)
     (for prob = (/ count %total-count))
     (sum (* prob (- 1.0 prob))))))
