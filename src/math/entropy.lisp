(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    entropy entropy-function

  (:range &key key test count-fn)

  (:range &key
          (key #'identity)
          (test 'eql)
          (count-fn (constantly 1)))

  (%table %total-count %count-fn)

  ((ensure-functionf count-fn)
   (setf %table (make-hash-table :test test)
         %count-fn count-fn
         %total-count 0))

  ((element)
   (let ((count (funcall %count-fn element)))
     (incf %total-count count)
     (incf (gethash element %table 0) count)))

  ((- (iterate
        (for (class count) in-hashtable %table)
        (for prob = (/ count %total-count))
        (assert (<= prob 1.0))
        (sum (* prob (log prob)))))))
