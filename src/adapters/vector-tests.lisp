(cl:in-package #:cl-user)
(defpackage vector-tests
  (:use #:cl #:prove #:cl-ds #:iterate #:alexandria))

(cl:in-package #:vector-tests)

(plan 14)
(let* ((vector (serapeum:vect 1 2 3 4 5 6 7))
       (range (cl-ds:whole-range vector)))
  (iterate
    (for (values val more) = (cl-ds:consume-front range))
    (while more)
    (for i from 0)
    (is (aref vector i) val))
  (reset! range)
  (iterate
    (for (values val more) = (cl-ds:consume-back range))
    (while more)
    (for i from (1- (length vector)) downto 0)
    (is (aref vector i) val)))
(finalize)
