(cl:in-package #:cl-data-structures.utils)


(declaim (inline xorshift))
(-> xorshift (integer integer) integer)
(defun xorshift (n i)
  (declare (optimize (speed 3) (safety 0)))
  (logxor n (ash n (- i))))


(declaim (inline hash-integer))
(-> hash-integer (integer) (unsigned-byte 64))
(defun hash-integer (n)
  "Attempts to randomize bits. Uses xorshift* algorithm."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (~> (xorshift n 12) (xorshift -25) (ldb (byte 64 0) _) (xorshift 27)
      (* #x2545F4914F6CDD1D) (ldb (byte 64 0) _)))
