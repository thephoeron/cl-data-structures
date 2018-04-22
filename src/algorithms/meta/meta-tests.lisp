(in-package #:cl-user)
(defpackage alg-meta-tests
  (:use #:common-lisp #:prove #:serapeum #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:alg-meta-tests)

(plan 5)

(let* ((vector1 #(1 2 3 4 5 6 7))
       (aggregator (cl-ds.alg.meta:construct-aggregator
                    nil
                    #'identity
                    #'cl-ds.alg:accumulate
                    nil
                    (list :fn #'+))))
  (cl-ds.alg.meta:begin-aggregation aggregator)
  (iterate
    (for elt in-vector vector1)
    (sum elt into result)
    (cl-ds.alg.meta:pass-to-aggregation aggregator elt)
    (finally
     (cl-ds.alg.meta:end-aggregation aggregator)
     (ok (cl-ds.alg.meta:aggregator-finished-p aggregator))
     (is (cl-ds.alg.meta:extract-result aggregator)
         result))))

(let* ((vector1 #(1 1 1 1 1 1 1))
       (aggregator (cl-ds.alg.meta:construct-aggregator
                    nil
                    #'identity
                    #'cl-ds.math:standard-deviation
                    nil
                    (list :key #'identity))))
  (iterate
    (until (cl-ds.alg.meta:aggregator-finished-p aggregator))
    (cl-ds.alg.meta:begin-aggregation aggregator)
    (until (cl-ds.alg.meta:aggregator-finished-p aggregator))
    (when (cl-ds.alg.meta:expects-content aggregator)
      (iterate
        (for elt in-vector vector1)
        (cl-ds.alg.meta:pass-to-aggregation aggregator elt)))
    (cl-ds.alg.meta:end-aggregation aggregator)
    (finally
     (is (cl-ds.alg.meta:extract-result aggregator)
         0.0))))

(let* ((vector1 #((1) (2) (1) (2) (1) (2) (1)))
       (proxy (~> vector1
                  cl-ds:whole-range
                  (cl-ds.alg:group-by :key (alexandria:compose #'evenp #'car)
                                      :test #'eq)
                  (cl-ds.alg:accumulate #'max _ :key #'car))))
  (is (cl-ds:at proxy t) 2)
  (is (cl-ds:at proxy nil) 1))


(let* ((vector1 #((1) (2) (1) (2) (1) (2) (1)))
       (proxy (~> vector1
                  cl-ds:whole-range
                  (cl-ds.alg:group-by :key (alexandria:compose #'evenp #'car)
                                      :test #'eq)
                  (cl-ds.math:standard-deviation :key #'car))))
  (is (cl-ds:at proxy t) 0)
  (is (cl-ds:at proxy nil) 0))

(finalize)
