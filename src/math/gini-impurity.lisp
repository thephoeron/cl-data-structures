(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    gini-impurity gini-impurity-function

  (:range classes &key key test)

  (:range classes &key (key #'identity) (test 'eql))

  (%table %total-count)

  ((&key test classes)
   (setf %table (make-hash-table :test test)
         %total-count 0)
   (map nil (lambda (class) (setf (gethash class %table) 0))
        classes))

  ((element)
   (when (gethash element %table)
     (incf %total-count)
     (incf (gethash element %table))))

  ((iterate
     (for (class count) in-hashtable %table)
     (for prob = (/ count %total-count))
     (sum (* prob (- 1.0 prob))))))
