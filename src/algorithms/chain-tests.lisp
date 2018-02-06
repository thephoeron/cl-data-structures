(in-package #:cl-user)
(defpackage chain-tests
  (:use #:cl #:prove #:serapeum #:cl-ds #:iterate #:alexandria)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:chain-tests)

(plan 38)

(let* ((vector1 (vect 1 2 3 4 5 6 7))
       (vector2 (vect 8 9 10 11 12))
       (range (cl-ds.alg:chain (cl-ds:whole-range vector1)
                               (cl-ds:whole-range vector2))))
  (is (cl-ds:size range) (+ (length vector1) (length vector2)))
  (iterate
    (for (values val more) = (cl-ds:consume-front range))
    (while more)
    (for i from 1)
    (is val i))
  (reset! range)
  (iterate
    (for (values val more) = (cl-ds:consume-back range))
    (while more)
    (for i from 12 downto 0)
    (is val i))
  (reset! range)
  (iterate
    (while (cl-ds:morep range))
    (for val = (cl-ds:consume-front range))
    (for i from 1)
    (is val i))
  (iterate
    (while (cl-ds:morep range))
    (for val = (cl-ds:consume-back range))
    (for i from 12 downto 0)
    (is val i))
  (is (cl-ds:size range) 0))

(finalize)
