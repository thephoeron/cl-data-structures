(in-package :cl-user)
(defpackage :chain-test-suite (:use :prove))
(in-package :chain-test-suite)
(cl-ds.utils:import-all-package-symbols :cl-ds.utils :chain-test-suite)

(prove:plan 5)

(let ((q (make-chain-queue)))
  (chain-queue-put 1 q)
  (chain-queue-put 2 q)
  (chain-queue-put 3 q)
  (chain-queue-put 4 q)
  (chain-queue-put 5 q)
  (chain-queue-put 6 q)
  (chain-queue-put 7 q)
  (chain-queue-put 8 q)
  (chain-queue-put 9 q)
  (chain-queue-put 10 q)
  (chain-queue-put 11 q)
  (chain-queue-put 12 q)
  (chain-queue-put 13 q)
  (chain-queue-put 14 q)
  (chain-queue-put 15 q)
  (chain-queue-put 16 q)
  (chain-queue-put 17 q)
  (let ((stack cl:nil))
    (chain-on-each (lambda (x) (push x stack)) q)
    (is stack (nreverse (iota 17 :start 1)) :test #'equal))
  (is (chain-queue-take q) 1)
  (is (chain-queue-take q) 2)
  (is (chain-queue-take q) 3)
  (is (chain-queue-take q) 4)
  (is (chain-queue-take q) 5)
  (is (chain-queue-take q) 6)
  (is (chain-queue-take q) 7)
  (is (chain-queue-take q) 8)
  (is (chain-queue-take q) 9)
  (is (chain-queue-take q) 10)
  (is (chain-queue-take q) 11)
  (is (chain-queue-take q) 12)
  (is (chain-queue-take q) 13)
  (is (chain-queue-take q) 14)
  (is (chain-queue-take q) 15)
  (is (chain-queue-take q) 16)
  (is (chain-queue-take q) 17))

(prove:finalize)
