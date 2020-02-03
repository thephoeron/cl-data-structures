(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    gini-impurity gini-impurity-function

    (:range &key key test size count-fn)

    (:range &key (key #'identity) (test 'eql)
     (size 16) (count-fn (constantly 1)))

    (%table %total-count %count-fn)

    ((ensure-functionf count-fn)
     (setf %table (make-hash-table :test test :size size)
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
