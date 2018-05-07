(in-package #:cl-user)
(defpackage without-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:without-tests)

(plan 4)

(let ((vector #(0 1 2 3 4 5 6 7 8 9 10 11))
      (result nil))
  (cl-ds:traverse (lambda (x)
                    (push x result))
                  (cl-ds.alg:without vector #'evenp))
  (is (sort result #'<) '(1 3 5 7 9 11) :test #'equal)
  (setf result (~> vector
                   (cl-ds.alg:without #'evenp)
                   (cl-ds.alg:group-by :key (lambda (x) (mod x 3)))
                   (cl-ds.alg:accumulate (flip #'cons) _ :initial-value nil)))
  (is (cl-ds:at result 0) '(9 3) :test #'equal)
  (is (cl-ds:at result 2) '(11 5) :test #'equal)
  (is (cl-ds:at result 1) '(7 1) :test #'equal))

(finalize)
