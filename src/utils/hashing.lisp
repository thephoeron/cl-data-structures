(cl:in-package #:cl-data-structures.utils)


(declaim (inline xorshift))
(-> xorshift (integer integer) integer)
(defun xorshift (n i)
  (declare (optimize (speed 3) (safety 0)))
  (logxor n (ash n (- i))))


(declaim (inline rol64))
(-> rol64 ((unsigned-byte 64) (unsigned-byte 64)) (unsigned-byte 64))
(defun rol64 (x k)
  (declare (optimize (speed 3) (safety 0)))
  (logior (ldb (byte 64 0) (ash x k))
          (ldb (byte 64 k) x)))


(declaim (inline hash-integer))
(-> hash-integer (integer &optional (unsigned-byte 64)) (values (unsigned-byte 64)
                                                                (unsigned-byte 64)))
(defun hash-integer (n &optional (multi #x2545F4914F6CDD1D))
  "Attempts to randomize bits. Uses xorshift* algorithm."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((new-state (~> (xorshift n 12) (xorshift -25) (ldb (byte 64 0) _)
                        (xorshift 27) (ldb (byte 64 0) _))))
    (values (ldb (byte 64 0) (* new-state multi))
            new-state)))


(declaim (inline xoshiro256**))
(defun xoshiro256** (state)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 64) (*)) state))
  (let ((result (ldb (byte 64 0) (* 9 (rol64 (ldb (byte 64 0)
                                                  (* 5 (aref state 1)))
                                             7))))
        (temp (ldb (byte 64 0) (ash (aref state 1) 17))))
    (macrolet ((sxor (first second)
                 `(setf (aref state ,first)
                        (logxor (aref state ,first)
                                (aref state ,second)))))
      (sxor 2 0)
      (sxor 3 1)
      (sxor 1 2)
      (sxor 0 3)
      (setf (aref state 2) (logxor (aref state 2) temp)
            (aref state 3) (rol64 (aref state 3) 45)))
    result))


(declaim (inline splitmix64))
(-> splitmix64 ((unsigned-byte 64)) (values (unsigned-byte 64)
                                            (unsigned-byte 64)))
(defun splitmix64 (state)
  (declare (optimize (speed 3) (safety 0)))
  (let ((new-state (ldb (byte 64 0) (+ state #x9E3779B97f4A7C15))))
    (values
     (~> (xorshift state 30)
         (* #xBF58476D1CE4E5B9) (ldb (byte 64 0) _)
         (xorshift 27)
         (* #x94D049BB133111EB) (ldb (byte 64 0) _)
         (xorshift 31))
     new-state)))

