(in-package #:cl-user)
(defpackage apriori-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria :metabang-bind)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:apriori-tests)

(plan 11)

(let* ((data #((1 2) (1 4) (1 2 4) (3 4)
               (1 3) (1 3) (1 3 4) (1 3 2)))
       (index (cl-ds.counting:apriori data 1))
       (vector (cl-ds.alg:to-vector (cl-ds.counting:all-sets index 0.1))))
  (is (cl-ds:size vector) 13)
  (is (length vector)
      (length (remove-duplicates vector
                                 :test 'equal
                                 :key #'cl-ds.counting:content)))
  (ok (every (compose (curry #'<= 1)
                      #'cl-ds.counting:support)
             vector))
  (let ((result (cl-ds.counting:find-association index '(1 3) '(4))))
    (ok result)
    (is (sort (cl-ds.counting:content result) #'<) '(1 3 4) :test #'equal)
    (is (sort (cl-ds.counting:content (cl-ds.counting:apriori-set result)) #'<) '(1 3) :test #'equal)
    (is (sort (cl-ds.counting:content (cl-ds.counting:aposteriori-set result)) #'<) '(4) :test #'equal)
    (is (cl-ds.counting:support result) 1)
    (is (cl-ds.counting:association-frequency result) 0.25 :test #'=)
    (is (cl-ds.counting:support (cl-ds.counting:find-set index 1 10)) 0)
    (is (cl-ds.alg:to-vector (cl-ds.alg:on-each #'cl-ds.counting:content
                                                (cl-ds.counting:all-super-sets (cl-ds.counting:find-set index 1 10)
                                                                               0.1)))
        (map 'vector #'cl-ds.counting:content vector)
        :test #'equalp)))

(finalize)
