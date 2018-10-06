(in-package :cl-user)
(defpackage transactional-dictionary-test-suite
  (:use :cl :prove :cl-ds :cl-data-structures.aux-package)
  (:export :run-stress-test :run-suite))
(in-package :transactional-dictionary-test-suite)

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
             (is (at v word) nil)))))))


(defmacro isolation-test (init-form limit)
  (once-only (limit)
    `(let ((dict ,init-form))
       (is (size dict) 0)
       (diag "filling up origingal dictionary")
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
       (let ((t-dict (become-transactional dict)))
         (diag "replacing items in the transactional dictionary")
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (ok (at t-dict word))
           (cl-ds:mod-bind (n rep old) (setf (at t-dict word) s)
             (ok rep)
             (is old word :test #'string=)))
         (diag "adding items to transactional dictionary")
         (iterate
           (for s from ,limit)
           (repeat ,limit)
           (for word = (aref *all-words* (1+ s)))
           (cl-ds:mod-bind (n rep old) (setf (at t-dict word) word)
             (is rep nil)
             (is old nil)))
         (iterate
           (for s from ,limit)
           (repeat ,limit)
           (for word = (aref *all-words* (1+ s)))
           (multiple-value-bind (v f) (at t-dict word)
             (is v word :test #'string=)
             (is f t)))
         (diag "Testing erase")
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (cl-ds:mod-bind (n e o) (erase! t-dict word)
             (ok e)
             (is o s)))
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (multiple-value-bind (v f) (at t-dict word)
             (is v nil)
             (is f nil)))
         (diag "Testing isolation from original container")
         (iterate
           (for s from ,limit)
           (repeat ,limit)
           (for word = (aref *all-words* (1+ s)))
           (multiple-value-bind (v f) (at dict word)
             (ok (not v))
             (ok (not f))))
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (multiple-value-bind (v f) (at dict word)
             (is v word :test #'string=)
             (ok f)))
         (cl-ds:reset! dict)
         (diag "Testing isolation between transactional instances")
         (let ((t-another-dict (become-transactional t-dict)))
           (iterate
             (for s from ,limit)
             (repeat ,limit)
             (for word in-vector *all-words*)
             (setf (at t-another-dict word) s))
           (iterate
             (for s from ,limit)
             (repeat ,limit)
             (for word in-vector *all-words*)
             (multiple-value-bind (value found) (at t-another-dict word)
               (is value s)
               (ok found)))
           (iterate
             (for s from ,limit)
             (repeat ,limit)
             (for word = (aref *all-words* (1+ s)))
             (setf (at t-another-dict word) 98789))
           (iterate
             (for s from ,limit)
             (repeat ,limit)
             (for word = (aref *all-words* (1+ s)))
             (multiple-value-bind (value found) (at t-another-dict word)
               (is value 98789)
               (ok found)))
           (iterate
             (for s from 1 below ,limit)
             (for word in-vector *all-words*)
             (multiple-value-bind (value found) (at t-dict word)
               (ok (not value))
               (ok (not found))))
           (iterate
             (for s from ,limit)
             (repeat ,limit)
             (for word = (aref *all-words* (1+ s)))
             (multiple-value-bind (value found) (at t-dict word)
               (is value word :test #'string=)
               (ok found))))))))


(let ((path (asdf:system-relative-pathname :cl-data-structures "test/dicts/result.txt")))
  (defun run-stress-test (limit)
    (with-open-file (str path :direction :output :if-exists :supersede)
      (let ((prove:*test-result-output* str))
        (format t "Running transactional HAMT tests, output redirected to ~a:~%" path)
        (diag "Running transactional HAMT tests:")
        (time (insert-every-word (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'string=) limit))))))


(defun run-suite ()
  (insert-every-word (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'string=) 2)
  (isolation-test (cl-ds:become-transactional (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'string=)) 100))


(progn
  (plan 3109)
  (run-suite)
  (finalize))
