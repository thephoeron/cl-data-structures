(in-package #:cl-data-structures.utils)


(-> lazy-shuffle (integer integer) function)
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


(-> draw-sample-vector (vector positive-fixnum &optional vector) vector)
(defun draw-sample-vector (input size
                           &optional (result (make-array (min size (length input))
                                                         :element-type (array-element-type input))))
  (when (array-has-fill-pointer-p result)
    (setf (fill-pointer result) size))
  (iterate
    (with generator = (lazy-shuffle 0 (length input)))
    (for i from 0 below (min size (length result)))
    (setf (aref result i) (aref input (funcall generator)))
    (finally (return result))))


(-> draw-random-vector (vector positive-fixnum &optional vector) vector)
(defun draw-random-vector (input size
                           &optional (result (make-array size
                                                         :element-type (array-element-type input))))
  (when (array-has-fill-pointer-p result)
    (setf (fill-pointer result) size))
  (let ((length (length input)))
    (map-into result (lambda () (aref input (random length))))))
