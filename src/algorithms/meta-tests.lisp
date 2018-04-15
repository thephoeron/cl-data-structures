(in-package #:cl-user)
(defpackage alg-meta-tests
  (:use #:common-lisp #:prove #:serapeum #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:alg-meta-tests)

(plan 1)

(let* ((vector1 #(1 2 3 4 5 6 7))
       (aggregator (cl-ds.alg.meta:construct-aggregator
                    vector1
                    (list (list* :result
                                 (lambda (range)
                                   (cl-ds.alg:accumulate #'+ range))))
                    nil
                    nil)))
  (cl-ds.alg.meta:begin-aggregation aggregator)
  (iterate
    (for elt in-vector vector1)
    (sum elt into result)
    (cl-ds.alg.meta:pass-to-aggregation aggregator elt)
    (finally
     (is (cl-ds.alg.meta:extract-result aggregator)
         result))))

(finalize)
