(in-package :cl-user)
(defpackage :chain-test-suite (:use :prove))
(in-package :chain-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-ds.utils :chain-test-suite)

(prove:plan 157)

(let ((q (make-chain-queue)))
  (iterate
    (for i from 0 below 80)
    (chain-queue-put i q))
  (let ((stack cl:nil))
    (chain-queue-on-each (lambda (x) (push x stack)) q)
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

(let ((q (make-double-chain-queue)))
  (ok (double-chain-queue-empty q))
  (double-chain-queue-put-back 1 q)
  (is (double-chain-queue-first q)
      (double-chain-queue-last q))
  (is (~> q double-chain-queue-first
          (aref (+ 2 +double-chain-size+)))
      1)
  (double-chain-queue-put-back 2 q)
  (is (~> q double-chain-queue-first
          (aref (+ 2 +double-chain-size+)))
      1)
  (is (~> q double-chain-queue-first
          (aref (+ 1 +double-chain-size+)))
      2)
  (double-chain-queue-put-front 3 q)
  (is (~> q double-chain-queue-first
          (aref 3))
      3))

(prove:finalize)
