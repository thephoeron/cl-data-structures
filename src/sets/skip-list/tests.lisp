(cl:in-package :cl-user)
(defpackage skip-list-set-tests
  (:use :cl :cl-ds :cl-data-structures.aux-package))
(in-package :skip-list-set-tests)


(prove:plan 5)

(let ((set (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=)))
  (prove:ok (not (cl-ds:at set 1)))
  (prove:is (cl-ds:size set) 0)
  (cl-ds:put! set 1)
  (prove:is (cl-ds:size set) 1)
  (prove:ok (cl-ds:at set 1))
  (prove:ok (not (cl-ds:at set 0)))
  )

(prove:finalize)
