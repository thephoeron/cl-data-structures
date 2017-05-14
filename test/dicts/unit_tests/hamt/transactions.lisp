(in-package :cl-user)
(defpackage transactional-hamt-dictionary-tests
  (:use :cl :prove :serapeum :cl-ds :iterate :alexandria :cl-ds.dicts.hamt)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export :run-stress-test
   :run-suite))
(in-package :transactional-hamt-dictionary-tests)

(setf prove:*enable-colors* nil)


(plan 56)
(diag "Testing isolation of insert")
(let ((dict (make-mutable-hamt-dictionary #'identity #'eql)))
  (iterate
    (for i below 8)
    (setf (at dict i) i))
  (iterate
    (for i from 64)
    (repeat 8)
    (setf (at dict i) i))
  (let ((trans-dict (become-transactional dict)))
    (iterate
      (for i from 8 below 16)
      (setf (at trans-dict i) i))
    (iterate
      (for i from 8 below 16)
      (is (at trans-dict i) i))
    (iterate
      (for i below 8)
      (ok (at trans-dict i)))
    (iterate
      (for i below 8)
      (is (at dict i) i :test #'eql))
    (iterate
      (for i from 8 below 16)
      (setf (at trans-dict i) 666))
    (iterate
      (for i from 8 below 16)
      (is (at trans-dict i) 666 :test #'eql)))
  (iterate
    (for i from 64)
    (repeat 8)
    (is  (at dict i) i))
  (iterate
    (for i below 8)
    (is (at dict i) i))
  (iterate
    (for i from 8 below 16)
    (ok (not (at dict i)))))
(finalize)
