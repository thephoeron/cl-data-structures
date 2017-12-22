(in-package :cl-user)
(defpackage :chain-test-suite (:use :prove))
(in-package :chain-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-ds.utils :chain-test-suite)

(prove:plan 151)

(let ((q (make-chain-queue)))
  (iterate
    (for i from 0 below 80)
    (chain-queue-put i q))
  (let ((stack cl:nil))
    (chain-on-each (lambda (x) (push x stack)) q)
    (is stack (nreverse (iota 80)) :test #'equal))
  (iterate
    (for i from 0 below 80)
    (is (chain-queue-take q) i))
  (is (chain-queue-first q) cl:nil)
  (is (chain-queue-last q) cl:nil)
  (is (chain-queue-first-count q) 0)
  (is (chain-queue-last-count q) 0)
  (iterate
    (for i from 0 below 65)
    (chain-queue-put i q))
  (is (chain-queue-take q) 0)
  (chain-queue-put 'a q)
  (is (chain-queue-take q) 1)
  (is (chain-queue-take q) 2)
  (iterate
    (for i from 3 below 65)
    (is (chain-queue-take q) i))
  (is (chain-queue-take q) 'a))

(prove:finalize)
