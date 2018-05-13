(in-package #:cl-data-structures.utils)


(defun lazy-shuffle (from to)
  (let ((table (make-hash-table))
        (index from))
    (lambda (&rest rest)
      (declare (ignore rest))
      (cond ((eql (- to 1) index)
             (ensure (gethash index table) index)
             (gethash (finc index) table))
            ((< index to)
             (let ((next-random (random-in-range index to)))
               (ensure (gethash index table) index)
               (ensure (gethash next-random table) next-random)
               (rotatef (gethash index table)
                        (gethash next-random table))
               (gethash (finc index) table)))
            (t nil)))))


(-> draw-sample-vector (vector positive-fixnum) vector)
(defun draw-sample-vector (input size)
  (iterate
    (with generator = (lazy-shuffle 0 (length input)))
    (with size = (min size (length input)))
    (with result = (make-array size
                               :element-type (array-element-type input)))
    (for i from 0 below size)
    (setf (aref result i) (aref input (funcall generator)))
    (finally (return result))))
