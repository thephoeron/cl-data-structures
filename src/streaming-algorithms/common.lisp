(in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)


(defun hashval (hashes width j hash)
  (~> (aref hashes j 0)
      (* hash)
      (+ (aref hashes j 1))
      (rem +long-prime+)
      (rem width)))


(defun make-hash-array (depth)
  (lret ((result (make-array (list depth 2) :element-type 'fixnum)))
    (map-into (cl-ds.utils:unfold-table result)
              (lambda () (truncate (1+ (/ (* (random most-positive-fixnum)
                                        +long-prime+)
                                     (1- most-positive-fixnum))))))))
