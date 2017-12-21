(in-package :cl-user)
(defpackage expression-tests
  (:use :cl :prove :serapeum :cl-ds :iterate :alexandria :cl-ds.dicts.hamt)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export :run-suite))
(in-package :expression-tests)

(plan 1)
(let ((data nil))
  (cl-ds:traverse (lambda (x) (push x data))
                  (cl-ds:xpr
                    (cl-ds:send 1)
                    (cl-ds:send 2)
                    (cl-ds:send 3)
                    (cl-ds:send 4)))
  (is data '(4 3 2 1) :test #'equal))
(finalize)
