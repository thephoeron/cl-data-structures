(in-package #:cl-ds.utils)


(defclass distance-matrix ()
  ((%size :type fixnum
          :reader read-size
          :initarg :size)
   (%content :type (simple-array single-float)
             :reader read-content
             :initarg :content)))


(defun index-in-content-of-distance-matrix (size row column)
  (declare (type index row column)
           (type positive-fixnum size)
           (optimize (speed 3) (safety 0)))
  (the index (+ (- (* size row)
                   (/ (* row (1+ row))
                      2)
                   1
                   row)
                column)))


(-> make-distance-matrix (vector (-> (t t) single-float)) distance-matrix)
(defun make-distance-matrix (sequence function)
  (let* ((size (length sequence))
         (result (make-array (1+ (index-in-content-of-distance-matrix size
                                                                      (1- size)
                                                                      (1- size)))
                             :element-type 'single-float
                             :initial-element 0.0)))
    (labels ((fill-matrix (column)
               (iterate
                 (with first = (aref sequence column))
                 (for row from (1+ column) below size)
                 (for second = (aref sequence row))
                 (setf (aref result (index-in-content-of-distance-matrix size
                                                                         column
                                                                         row))
                       (funcall function first second)))))
      (~>>
       (iterate
         (for i from 0 below size)
         (let ((i i))
           (collect (lparallel:future (fill-matrix i)) at start)))
       (map nil #'lparallel:force))
      (assure distance-matrix
           (make 'distance-matrix
                 :size (length sequence)
                 :content result)))))


(defgeneric distance (matrix from to)
  (:method ((matrix distance-matrix) from to)
    (declare (type index from to)
             (optimize (speed 3)))
    (let ((size (slot-value matrix '%size)))
      (declare (type index size))
      (cond
        ((or (>= from size) (>= to size))
         (error "No such position in the matrix."))
        ((eql from to)
         0.0)
        (t (let ((content (slot-value matrix '%content))
                 (from (min from to))
                 (to (max from to)))
             (declare (type (simple-array single-float) content))
             (aref content (index-in-content-of-distance-matrix size
                                                                from
                                                                to))))))))


(defgeneric each-in-matrix (matrix fn)
  (:method ((matrix distance-matrix) fn)
    (declare (type (-> (index index single-float) t) fn))
    (with-vectors ((content (slot-value matrix '%content)))
      (iterate
        (with size = (slot-value matrix '%size))
        (with index = 0)
        (for i from 0 below size)
        (iterate
          (for j from (1+ i) below size)
          (funcall fn i j (content index))
          (incf index)))
      matrix)))
