(cl:in-package #:cl-user)
(defpackage sequence-window-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:sum #:collecting #:summing #:in))

(cl:in-package #:sequence-window-tests)

(plan 2)

(let* ((vector #(0 1 2 3 4 5 6 7 8 9 10 11 12))
       (range (cl-ds.common:sequence-window vector 0 13))
       (collection nil)
       (window-range (cl-ds.common:sequence-window vector 4 10)))
  (cl-ds:across range
                (lambda (x) (push x collection)))
  (is (sort collection #'<) (iota 13) :test #'equal)
  (setf collection nil)
  (cl-ds:across window-range
                (lambda (x) (push x collection)))
  (is (sort collection #'<) (iota 6 :start 4) :test #'equal))

(finalize)
