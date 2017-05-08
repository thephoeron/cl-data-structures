(in-package :cl-user)
(defpackage functional-dictionary-test-suite
  (:use :cl :prove :serapeum :cl-ds :iterate :alexandria)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export :run-stress-test
   :run-suite
   :test-destructive-insert))
(in-package :hamt-tests)

(setf prove:*enable-colors* nil)

