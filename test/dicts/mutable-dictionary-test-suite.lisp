(in-package :cl-user)
(defpackage mutable-dictionary-test-suite
  (:use :cl :prove :serapeum :cl-ds :iterate :alexandria)
  (:shadowing-import-from :iterate :collecting :summing :in)
  (:export :run-stress-test
   :run-suite))
(in-package :mutable-dictionary-test-suite)

(setf prove:*enable-colors* nil)

(let ((path (asdf:system-relative-pathname :cl-data-structures "test/files/words.txt")))
  (defun read-all-words ()
    (let ((result (vect)))
      (with-open-file (str path)
        (iterate
          (for word = (read-line str nil nil))
          (while word)
          (vector-push-extend word result)))
      result)))

(defvar *all-words* (read-all-words))

(defmacro insert-every-word (init-form limit)
  (once-only (limit)
    `(let ((dict ,init-form))
       (is (size dict) 0)
       (ok (cl-ds:emptyp dict))
       (diag "Testing insert")
       (iterate
         (for s from 1 below ,limit)
         (for word in-vector *all-words*)
         (ok (not (cl-ds:at dict word)))
         (cl-ds:mod-bind (next replaced old) (setf (at dict word) word)
           (is replaced nil)
           (is old nil))
         (multiple-value-bind (v f) (at dict word)
           (is v word :test #'string=)
           (ok f))
         (is (size dict) s))
       (diag "Testing at")
       (iterate
         (for word in-vector *all-words*)
         (for s from 1 below ,limit)
         (multiple-value-bind (v f) (at dict word)
           (is v word :test #'string=)
           (ok f)))
       (diag "Testing update")
       (iterate
         (for word in-vector *all-words*)
         (for s from 1 below ,limit)
         (cl-ds:mod-bind (v u o) (update! dict word word)
           (is o word :test #'string=)
           (ok u)))
       (diag "Testing add")
       (iterate
         (for s from 1 below ,limit)
         (for word in-vector *all-words*)
         (let ((old-size (size dict)))
           (cl-ds:mod-bind (v a) (add! dict word word)
             (is a t)
             (is (size dict) old-size)
             (is v dict))))
       (diag "Testing add")
       (iterate
         (for s from ,limit)
         (repeat ,limit)
         (while (< s (fill-pointer *all-words*)))
         (for word = (aref *all-words* s))
         (let ((old-size (size dict)))
           (cl-ds:mod-bind (v a) (add! dict word word)
             (is a nil)
             (is (size dict) (1+ old-size)))))
       (iterate
         (for s from ,limit)
         (repeat ,limit)
         (while (< s (fill-pointer *all-words*)))
         (for word = (aref *all-words* s))
         (let ((old-size (size dict)))
           (cl-ds:mod-bind (v a) (add! dict word word)
             (is a t)
             (is (size dict) old-size))))
       (diag "Testing erase")
       (iterate
         (for s from 1 below ,limit)
         (for word in-vector *all-words*)
         (let ((old-size (size dict)))
           (cl-ds:mod-bind (v r o) (erase! dict word)
             (ok r)
             (is o word :test #'string=)
             (is (size dict) (1- old-size))
             (is v dict)
             (is (at v word) nil))))
       dict)))


(let ((path (asdf:system-relative-pathname :cl-data-structures "test/dicts/result.txt")))
  (defun run-stress-test (limit)
    (with-open-file (str path :direction :output :if-exists :supersede)
      (let ((prove:*test-result-output* str)
            (dict nil)
            (hashtable (make-hash-table :test 'equal)))
        (format t "Running mutable HAMT tests, output redirected to ~a:~%" path)
        (diag "Running mutable HAMT tests:")
        (time (setf dict (insert-every-word (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'string=) limit)))
        (iterate
          (for s from 1 below limit)
          (for word in-vector *all-words*)
          (setf (gethash word hashtable) word))
        (time
         (iterate
           (for s from 1 below limit)
           (for word in-vector *all-words*)
           (gethash word hashtable)))
        (time
         (iterate
           (for s from 1 below limit)
           (for word in-vector *all-words*)
           (cl-ds:at dict word)))))))
  


(defun run-suite ()
  (plan 28)
  (insert-every-word (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'equal) 2)
  (finalize))

