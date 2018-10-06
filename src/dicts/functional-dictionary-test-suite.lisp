(in-package :cl-user)
(defpackage functional-dictionary-test-suite
  (:use :cl :prove :cl-ds :cl-data-structures.aux-package)
  (:export :run-stress-test
   :run-suite))
(in-package :functional-dictionary-test-suite)

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
         (cl-ds:mod-bind (next replaced old) (cl-ds:insert dict word word)
           (setf dict next)
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
         (cl-ds:mod-bind (v u o) (update dict word word)
           (is o word :test #'string=)
           (ok u)
           (setf dict v)))
       (diag "Testing add")
       (iterate
         (for s from 1 below ,limit)
         (for word in-vector *all-words*)
         (cl-ds:mod-bind (v a) (add dict word word)
           (is a t)
           (is (size dict) (size v))))
       (diag "Testing add")
       (iterate
         (for s from ,limit)
         (repeat ,limit)
         (while (< s (fill-pointer *all-words*)))
         (for word = (aref *all-words* s))
         (cl-ds:mod-bind (v a) (add dict word word)
           (is a nil)
           (is (1+ (size dict)) (size v))
           (setf dict v)))
       (iterate
         (for s from ,limit)
         (repeat ,limit)
         (while (< s (fill-pointer *all-words*)))
         (for word = (aref *all-words* s))
         (cl-ds:mod-bind (v a) (add dict word word)
           (is a t)
           (is (size dict) (size v))
           (setf dict v)))
       (diag "Testing erase")
       (trivial-garbage:gc :full t)
       (let ((dict dict))
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (cl-ds:mod-bind (v r o) (erase dict word)
             (ok r)
             (is o word :test #'string=)
             (is (1- (size dict)) (size v))
             (is nil (at v word))
             (setf dict v))))
       (diag "Testing erase if.")
       (let ((dict dict))
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (cl-ds:mod-bind (v r o) (erase-if dict word (lambda (key value)
                                                         (is key word :test #'string=)
                                                         (is value word :test #'string=)
                                                         t))
             (ok r)
             (is o word :test #'string=)
             (is (1- (size dict)) (size v))
             (is nil (at v word))
             (setf dict v))))
       (let ((dict dict))
         (iterate
           (for s from 1 below ,limit)
           (for word in-vector *all-words*)
           (cl-ds:mod-bind (v r o) (erase-if dict word (lambda (key value)
                                                         (is key word :test #'string=)
                                                         (is value word :test #'string=)
                                                         nil))
             (ok (null r))
             (is o nil)
             (is (size dict) (size v))
             (is word (at v word) :test #'string=)))))))

(defun run-suite ()
  (diag "Testing functional implementation.")
  (insert-every-word (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'string=) 100)
  (diag "Testing lazy implementation.")
  (insert-every-word (cl-ds:become-lazy (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'string=))
                     100))


(let ((path (asdf:system-relative-pathname :cl-data-structures "test/dicts/result.txt")))
  (defun run-stress-test (limit)
    (with-open-file (str path :direction :output :if-exists :supersede)
      (let ((prove:*test-result-output* str))
        (format t "Running functional HAMT tests, output redirected to ~a:~%" path)
        (format t "Running functional HAMT tests:")
        (time (insert-every-word (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'string=) limit))
        (format t "Running lazy HAMT tests:")
        (time (insert-every-word (cl-ds:become-lazy (cl-ds.dicts.hamt:make-functional-hamt-dictionary
                                                     #'sxhash
                                                     #'string=))
                                 limit))))))


(progn
  (plan 6346)
  (run-suite)
  (finalize))
