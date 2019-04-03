(in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)


(defun hashval (hashes width j hash)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* 2)) hashes)
           (type non-negative-fixnum width j hash))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 32 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 32 0) _)
      (rem +long-prime+)
      (rem width)))


(defun make-hash-array (count)
  (lret ((result (make-array (list count 2) :element-type 'fixnum)))
    (map-into (cl-ds.utils:unfold-table result)
              (curry #'random most-positive-fixnum))))


(defclass fundamental-data-sketch ()
  ())


(defgeneric compatible-p (first-sketch &rest more-sketches))

(defgeneric union (first-sketch &rest more-sketches))
