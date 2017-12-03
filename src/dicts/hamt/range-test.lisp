(in-package :cl-user)
(defpackage hamt-range-tests
  (:use :cl :prove :serapeum :cl-ds :iterate :alexandria :cl-ds.dicts.hamt)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export :run-suite))
(in-package :hamt-range-tests)

(plan 8)
(let ((dict (make-mutable-hamt-dictionary #'identity #'eql))
      (count 0))
  (setf (cl-ds:at dict 5) 1)
  (setf (cl-ds:at dict 6) 2)
  (setf (cl-ds:at dict 7) 3)
  (setf (cl-ds:at dict 8) 4)
  (let ((range (cl-ds:whole-range dict)))
    (iterate
      (while (cl-ds:morep range))
      (for key.value = (cl-ds:consume-front range))
      (incf count)
      (collect key.value into result)
      (finally (setf result (sort result #'< :key #'car))
               (prove:is result
                         '((5 . 1) (6 . 2) (7 . 3) (8 . 4))
                         :test #'equal)
               (prove:is count 4)
               (prove:ok (not (cl-ds:morep range))))))
  (let ((range (cl-ds:whole-range dict)))
    (iterate
      (repeat 4)
      (for key.value = (cl-ds:peek-front range))
      (collect key.value into result)
      (finally (setf result (sort result #'< :key #'car))
               (prove:is result
                         '((5 . 1) (5 . 1) (5 . 1) (5 . 1))
                         :test #'equal)
               (prove:ok (cl-ds:morep range)))))
  (let ((sum (~> dict
                 cl-ds:whole-range
                 (cl-ds:accumulate #'+ _ :key #'cdr))))
    (is sum 10))
  (let ((divided-sum (~> dict
                         cl-ds:whole-range
                         (cl-ds:group-by :key (compose #'evenp #'cdr))
                         (cl-ds:accumulate #'+ _ :key #'cdr))))
    (is (cl-ds:at divided-sum nil) 4)
    (is (cl-ds:at divided-sum t) 6)))
(finalize)
