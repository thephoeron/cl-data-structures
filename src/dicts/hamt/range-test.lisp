(in-package :cl-user)
(defpackage hamt-range-tests
  (:use :cl :prove :cl-ds :cl-ds.dicts.hamt
        :cl-data-structures.aux-package)
  (:export :run-suite))
(in-package :hamt-range-tests)

(plan 18)
(let ((dict (make-mutable-hamt-dictionary #'identity #'eql))
      (count 0))
  (setf (cl-ds:at dict 5) 1)
  (setf (cl-ds:at dict 6) 2)
  (setf (cl-ds:at dict 7) 3)
  (setf (cl-ds:at dict 8) 4)
  (let ((range (cl-ds:whole-range dict)))
    (iterate
      (for (values key.value more) = (cl-ds:consume-front range))
      (while more)
      (incf count)
      (collect key.value into result)
      (finally (setf result (sort result #'< :key #'car))
               (prove:is result
                         '((5 . 1) (6 . 2) (7 . 3) (8 . 4))
                         :test #'equal)
               (prove:is count 4))))
  (let ((range (cl-ds:whole-range dict)))
    (iterate
      (repeat 4)
      (for key.value = (cl-ds:peek-front range))
      (collect key.value into result)
      (finally (setf result (sort result #'< :key #'car))
               (prove:is result
                         '((5 . 1) (5 . 1) (5 . 1) (5 . 1))
                         :test #'equal))))
  (let ((sum (~> dict
                 cl-ds:whole-range
                 (cl-ds.alg:accumulate #'+ _ :key #'cdr))))
    (is sum 10))
  (let ((sum (~> dict
                 cl-ds:whole-range
                 (cl-ds.alg:on-each (lambda (x) (* (cdr x) 2)) _)
                 (cl-ds.alg:accumulate #'+ _))))
    (is sum 20))
  (let ((sum (~> dict
                 cl-ds:whole-range
                 (cl-ds.alg:on-each (curry #'* 2) _ :key #'cdr)
                 (cl-ds.alg:accumulate #'+ _))))
    (is sum 20))
  (let ((divided-sum (~> dict
                         cl-ds:whole-range
                         (cl-ds.alg:group-by :key (compose #'evenp #'cdr))
                         (cl-ds.alg:accumulate #'+ _ :key #'cdr))))
    (is (cl-ds:at divided-sum nil) 4)
    (is (cl-ds:at divided-sum t) 6))
  (let ((divided-variance (~> dict
                              cl-ds:whole-range
                              (cl-ds.alg:group-by :key (compose #'evenp #'cdr))
                              (cl-ds.math:variance :key #'cdr))))
    (is (cl-ds:at divided-variance nil) 1)
    (is (cl-ds:at divided-variance t) 1))
  (let ((range (cl-ds:whole-range dict))
        (result nil))
    (setf (cl-ds:peek-front range) 'value)
    (is (cdr (cl-ds:peek-front range)) 'value)
    (cl-ds:traverse (lambda (x) (push x result))
                    range)
    (is (cdr (find 'value result :key #'cdr)) 'value)))

(let ((dict (cl-ds:make-from-traversable
             'cl-ds.dicts.hamt:mutable-hamt-dictionary
             (cl-ds.alg:chain
              (cl-ds:whole-range #((1 . 1) (2 . 2) (3 . 3)))
              (cl-ds:whole-range #((4 . 4) (5 . 5) (6 . 6))))
             :hash-fn #'identity
             :equal-fn #'=)))
  (iterate
    (for i from 1 to 6)
    (is (cl-ds:at dict i) i)))

(finalize)
