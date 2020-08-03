(cl:in-package #:cl-data-structures.utils)


(declaim (inline xorshift))
(-> xorshift (integer integer) integer)
(defun xorshift (n i)
  (declare (optimize (speed 3) (safety 0)))
  (logxor n (ash n (- i))))


(declaim (inline hash-integer))
(-> hash-integer (integer) integer)
(defun hash-integer (n)
  "Attempts to randomize bit positions."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (~> n
      (xorshift 32) (* #x45d9f3b) (ldb (byte 64 0) _)
      (xorshift 16) (* #x45d9f3b) (ldb (byte 64 0) _)
      (xorshift 8)))

