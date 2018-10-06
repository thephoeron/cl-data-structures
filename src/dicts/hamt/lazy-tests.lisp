(in-package :cl-user)
(defpackage lazy-hamt-dictionary-tests
  (:use :cl :prove :cl-ds :cl-ds.dicts.hamt
        :cl-data-structures.aux-package))
(in-package :lazy-hamt-dictionary-tests)

(plan 1114)
(diag "Testing lazy HAMT")
(let ((dict (become-lazy (make-mutable-hamt-dictionary #'identity #'eql))))
  (diag "testing insert into empty")
  (iterate
    (for i below 64)
    (for next = (insert dict i i)))
  (is (cl-ds:size dict) 0)
  (iterate
    (for i below 64)
    (for next = (insert dict i i))
    (is (at next i) i))
  (is (cl-ds:size dict) 0)

  (iterate
    (for i below 256)
    (for next = (insert dict i i)))
  (is (cl-ds:size dict) 0)
  (iterate
    (for i below 256)
    (for next = (insert dict i i))
    (is (at next i) i))
  (is (cl-ds:size dict) 0))

(let ((mutable (make-mutable-hamt-dictionary #'identity #'eql))
      (dict nil))
  (iterate
    (for i below 8)
    (setf (at mutable i) 'correct))
  (setf dict (become-lazy mutable))
  (diag "Testing insert into not empty")
  (iterate
    (for i below 64)
    (for next = (insert dict i i)))
  (is (size mutable) 8)

  (iterate
    (for i below 64)
    (for next = (insert dict i i))
    (is (at next i) i))
  (is (size dict) 8)
  (iterate
    (for i below 8)
    (is (at mutable i) 'correct))
  (iterate
    (for i below 256)
    (for next = (insert dict i i)))
  (is (size dict) 8)
  (diag "Testing delayed insert.")
  (let ((n-dict dict))
    (iterate
      (for i below 256)
      (setf n-dict (insert n-dict i i)))
    (diag "Testing forcing.")
    (iterate
      (for i below 256)
      (is (at n-dict i) i))
    (diag "Testing removing")
    (let ((p-dict n-dict))
      (iterate
        (for i below 128)
        (setf p-dict (erase p-dict i)))
      (iterate
        (for i below 64)
        (is (at p-dict i) nil))
      (iterate
        (for i below 256)
        (is (at n-dict i) i))))
  (is (size mutable) 8)
  (iterate
    (for i below 8)
    (is (at mutable i) 'correct)))

(let ((mutable (make-mutable-hamt-dictionary #'identity #'eql))
      (dict nil)
      (dict2 nil))
  (setf dict (become-lazy mutable))
  (diag "Testing many instances into not empty")
  (iterate
    (for i below 64)
    (for next = (insert dict i 'correct))
    (setf dict next))
  (is (size dict) 64)

  (setf dict2 dict)

  (iterate
    (for i from 64 below 256)
    (for next = (insert dict i i))
    (setf dict next))
  (is (size dict2) 64)
  (iterate
    (for i below 64)
    (is (at dict i) 'correct))

  (iterate
    (for i below 64)
    (for next = (update dict i 'next))
    (setf dict next))

  (iterate
    (for i below 64)
    (is (at dict i) 'next))

  (iterate
    (for i below 64)
    (for next = (update dict i 'next))
    (setf dict next)))

(finalize)
