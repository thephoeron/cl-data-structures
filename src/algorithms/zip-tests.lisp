(in-package #:cl-user)
(defpackage zip-tests
  (:use :cl :prove :cl-data-structures.aux-package))

(in-package #:zip-tests)

(plan 6)

(let* ((vector1 #(1 2 3 4 5 6 7))
       (vector2 #(8 9 10 11 12))
       (range (cl-ds.alg:zip
               #'list*
               (cl-ds:whole-range vector1)
               (cl-ds:whole-range vector2))))
  (is (cl-ds:size range) (min (length vector1) (length vector2)))
  (iterate
    (for (values value more) = (cl-ds:consume-front range))
    (while more)
    (for v1 in-vector vector1)
    (for v2 in-vector vector2)
    (is value (list* v1 v2) :test #'equal)))

(finalize)
