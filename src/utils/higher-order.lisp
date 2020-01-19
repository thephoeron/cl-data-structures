(cl:in-package #:cl-ds.utils)


(defun or* (&rest functions)
  (lambda (&rest rest)
    (iterate
      (for function in functions)
      (for result = (apply function rest))
      (when result
        (leave result)))))


(defun and* (&rest functions)
  (lambda (&rest rest)
    (iterate
      (for function in functions)
      (for result = (apply function rest))
      (always result)
      (finally (return result)))))


(defun if-else (predicate true false)
  (ensure-functionf predicate true false)
  (lambda (&rest all)
    (if (apply predicate all)
        (apply true all)
        (apply false all))))


(defun cycle-over-address (dimensions &rest pinned)
  (bind ((address (make-array (length dimensions)
                              :element-type 'fixnum
                              :initial-element 0))
         (length (length dimensions))
         (skipped 0)
         (current-address 0)
         (total-count 1)
         (result nil))
    (when (oddp (length pinned))
      (error "Passed odd number of arguments as dimensions to pin"))
    (iterate
      (with batches = (batches pinned 2))
      (for (axis position) in batches)
      (setf (ldb (byte 1 axis) skipped) 1)
      (setf (elt address axis) position)
      (finally (decf length (length batches))))
    (iterate
      (for i from 0)
      (for dim in dimensions)
      (unless (ldb-test (byte 1 i) skipped)
        (setf total-count (* total-count dim))))
    (setf result (coerce address 'list))
    (setf dimensions (coerce dimensions '(vector fixnum)))
    (lambda ()
      (unless (zerop total-count)
        (iterate
          (for k from 0 below length)
          (for i = (+ k (ldb (byte (1+ k) 0) skipped)))
          (for pos = (aref address i))
          (for dim = (aref dimensions i))
          (when (eql i current-address)
            (if (< (1+ pos) dim)
                (progn
                  (incf (aref address i))
                  (leave))
                (progn
                  (iterate
                    (for s from 0 below i)
                    (for sub = (+ s (ldb (byte (1+ s) 0) skipped)))
                    (setf (aref address sub) 0))
                  (incf current-address)
                  (next-iteration))))
          (when (< (1+ pos) dim)
            (incf (aref address i))
            (setf current-address i)
            (leave)))
        (decf total-count)
        (shiftf result (coerce address 'list))))))


(defun ignore-errors* (function)
  (lambda (&rest all)
    (ignore-errors (apply function all))))


(defun generator (function initial-state)
  (ensure-functionf function)
  (lambda (&rest all)
    (declare (ignore all))
    (shiftf initial-state (funcall function initial-state))))


(defun prevent-duplicates (&key (test 'eql))
  (let ((table (make-hash-table :test test)))
    (lambda (data)
      (ensure (gethash data table) data))))
