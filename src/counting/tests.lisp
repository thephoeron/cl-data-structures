(in-package #:cl-user)
(defpackage apriori-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:apriori-tests)

(plan 3)

(let* ((data #((1 2) (1 4) (1 2 4) (3 4)
               (1 3) (1 3) (1 3 4) (1 3 2)))
       (index (cl-ds.counting:apriori data
                                      1
                                      0.1))
       (result (cl-ds.alg:to-vector (cl-ds:whole-range index))))
  (is (cl-ds:size (cl-ds.alg:to-vector result)) 19)
  (ok (every (compose (rcurry #'>= 0.1) #'first) result))
  (is (length result) (~> (map 'vector #'rest result)
                          (remove-duplicates :test #'equal)
                          length)))

(finalize)
