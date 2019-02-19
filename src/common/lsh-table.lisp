(in-package #:cl-user)


(defpackage :cl-data-structures.common.lsh
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.lsh)
  (:export))


(in-package #:cl-data-structures.common.lsh)


(defun generate-projection (row-count column-count gauss-min gauss-max)
  (lret ((result (make-array `(,row-count ,column-count)
                             :initial-element 0.0
                             :element-type 'single-float)))
    (iterate
      (for i from 0 below (array-total-size result))
      (setf (row-major-aref result i) (alexandria:gaussian-random
                                       gauss-min
                                       gauss-max)))))


(defun get-random-projection-lsh (vector projection)
  (declare (type (vector single-float) vector)
           (type (simple-array single-float (* *)) projection))
  (let* ((projection-size (array-dimension projection 0))
         (column-count (array-dimension projection 1))
         (hashes-size (ceiling projection-size 32))
         (hashes (make-array hashes-size :element-type 'fixnum
                             :initial-element 0)))
    (iterate
      (for row from 0 below projection-size)
      (for dot-value = (iterate
                         (for column from 0 below column-count)
                         (sum (* (aref vector column)
                                 (aref projection row column)))))
      (when (>= dot-value 0)
        (bind (((:values index bit) (floor row 32)))
          (setf (ldb (byte 1 bit) (aref hashes index)) 1))))
    (reduce (lambda (prev next)
              (logior (ash prev 32) next))
            hashes)))
