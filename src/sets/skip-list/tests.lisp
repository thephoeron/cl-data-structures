(cl:in-package :cl-user)
(defpackage skip-list-set-tests
  (:use :cl :cl-ds :cl-data-structures.aux-package))
(in-package :skip-list-set-tests)


(prove:plan 365)

(let ((set (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=)))
  (prove:ok (not (cl-ds:at set 1)))
  (prove:is (cl-ds:size set) 0)
  (cl-ds:put! set 1)
  (prove:is (cl-ds:size set) 1)
  (prove:ok (cl-ds:at set 1))
  (prove:ok (not (cl-ds:at set 0))))

(iterate
  (with set = (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=))
  (with data = (iota 15))
  (for i from 0)
  (for d on data)
  (iterate
    (for k on data)
    (until (eq d k))
    (prove:ok (cl-ds:at set (first k))))
  (prove:is (cl-ds:size set) i)
  (cl-ds:put! set (first d))
  )

(iterate
  (with set = (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=))
  (with data = (reverse (iota 15)))
  (for i from 0)
  (for d on data)
  (iterate
    (for k on data)
    (until (eq d k))
    (prove:ok (cl-ds:at set (first k))))
  (prove:is (cl-ds:size set) i)
  (cl-ds:put! set (first d))
  )

(iterate
  (with set = (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=))
  (with data = (shuffle (iota 15)))
  (for i from 0)
  (for d on data)
  (iterate
    (for k on data)
    (until (eq d k))
    (prove:ok (cl-ds:at set (first k))))
  (prove:is (cl-ds:size set) i)
  (cl-ds:put! set (first d))
  )

(prove:finalize)
