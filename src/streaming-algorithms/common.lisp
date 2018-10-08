(in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)


(defun hashval (hashes width j hash)
  (~> (aref hashes j 0)
      (* hash)
      (+ (aref hashes j 1))
      (rem +long-prime+)
      (rem width)))


(defun make-hash-array (count)
  (lret ((result (make-array (list count 2) :element-type 'fixnum)))
    (map-into (cl-ds.utils:unfold-table result)
              (curry #'random most-positive-fixnum))))
