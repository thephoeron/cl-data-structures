(in-package #:cl-user)
(defpackage apriori-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:apriori-tests)

(plan 3)

(let* ((data #((1 2) (1 4) (1 2 4) (3 4)
               (1 3) (1 3) (1 3 4) (1 3 2)))
       (index (cl-ds.counting:apriori data 1))
       (result (cl-ds.alg:to-vector (cl-ds.counting:all-sets index 0.1))))
  (is (cl-ds:size result) 19)
  (is (length result)
      (length (remove-duplicates result
                                 :test 'equal
                                 :key #'cl-ds.counting:content)))
  (ok (every (compose (curry #'<= 1)
                      #'cl-ds.counting:support)
             result)))

(finalize)
