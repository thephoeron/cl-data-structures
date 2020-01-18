(in-package #:cl-user)
(defpackage alg-meta-tests
  (:use #:common-lisp #:prove #:cl-data-structures.aux-package)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in))

(in-package #:alg-meta-tests)

(plan 6)

(let* ((vector1 #((1) (2) (1) (2) (1) (2) (1)))
       (proxy (~> vector1
                  cl-ds:whole-range
                  (cl-ds.alg:group-by :key (alexandria:compose #'evenp #'car)
                                      :test #'eq)
                  (cl-ds.alg:accumulate #'max :key #'car))))
  (is (cl-ds:at proxy t) 2)
  (is (cl-ds:at proxy nil) 1))


(let* ((vector1 #((6 . 1) (6 . 1) (6 . 1)
                  (5 . 2) (5 . 2) (5 . 2)
                  (5 . 3) (5 . 3) (5 . 3)
                  (6 . 4) (6 . 4) (6 . 4)))
       (proxy (~> vector1
                  cl-ds:whole-range
                  (cl-ds.alg:group-by :key (alexandria:compose #'evenp #'car)
                                      :test #'eq)
                  (cl-ds.alg:group-by :key (alexandria:compose #'evenp #'cdr)
                                      :test #'eq)
                  (cl-ds.alg:accumulate #'+ :key #'cdr))))
  (is (~> proxy (cl-ds:at t) (cl-ds:at t)) 12 :test #'=)
  (is (~> proxy (cl-ds:at nil) (cl-ds:at t)) 6 :test #'=)
  (is (~> proxy (cl-ds:at nil) (cl-ds:at nil)) 9 :test #'=)
  (is (~> proxy (cl-ds:at t) (cl-ds:at nil)) 3 :test #'=))

(finalize)
