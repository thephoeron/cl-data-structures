(in-package #:cl-ds.utils)


(defclass distance-matrix ()
  ((%size :type fixnum
          :reader read-size
          :initarg :size)
   (%content :type simple-array
             :reader read-content
             :initarg :content)))


(defgeneric mutate-matrix (destination fn m1 m2)
  (:method ((destination distance-matrix) fn (m1 distance-matrix) (m2 distance-matrix))
    (let ((dest (read-content destination))
          (s1 (read-content m1))
          (s2 (read-content m2)))
      (unless (= (read-size destination) (read-size m1) (read-size m2))
        (error "Sizes don't mach!"))
      (lparallel:pmap-into dest fn s1 s2)
      destination)))


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


(defun make-distance-matrix (type size)
  (let ((result (make-array (1+ (index-in-content-of-distance-matrix size
                                                                     (1- size)
                                                                     (1- size)))
                            :element-type type)))
    (assure distance-matrix
      (make 'distance-matrix
            :size size
            :content result))))


(-> make-distance-matrix-from-vector ((or list symbol) vector (-> (t t) number) &key (:key (-> (t) t))) distance-matrix)
(defun make-distance-matrix-from-vector (type sequence function &key (key #'identity))
  (let* ((size (length sequence))
         (result (make-array (1+ (index-in-content-of-distance-matrix size
                                                                      (1- size)
                                                                      (1- size)))
                             :element-type type)))
    (labels ((fill-matrix (column)
               (iterate
                 (with first = (aref sequence column))
                 (for row from (1+ column) below size)
                 (for second = (aref sequence row))
                 (setf (aref result (index-in-content-of-distance-matrix size
                                                                         column
                                                                         row))
                       (funcall function (funcall key first) (funcall key second))))))
      (declare (inline fill-matrix))
      (iterate
        (for i from 0 below size)
        (fill-matrix i))
      (assure distance-matrix
        (make 'distance-matrix
              :size size
              :content result)))))


(-> parallel-make-distance-matrix-from-vector ((or list symbol) function vector &key (:key (-> (t) t)) (:function-context (-> (t) t))) distance-matrix)
(defun parallel-make-distance-matrix-from-vector (type function sequence &key (key #'identity) (function-context #'identity))
  (declare (optimize (speed 3)))
  (let* ((size (length sequence))
         (result (make-array (1+ (index-in-content-of-distance-matrix size
                                                                      (1- size)
                                                                      (1- size)))
                             :element-type type))
         (indexes (iterate
                    (for i below size)
                    (collect i into forward at start)
                    (collect i into backward)
                    (finally (return (map '(vector list) #'list backward forward))))))
    (lparallel:pmap
     nil
     (lambda (index)
       (fbind ((dist-function (funcall function-context function)))
         (iterate
           (for i in index)
           (for x = (aref sequence i))
           (iterate
             (for j from (1+ i) below size)
             (for y = (aref sequence j))
             (setf (aref result (index-in-content-of-distance-matrix size i j))
                   (dist-function (funcall key x) (funcall key y)))))))
     indexes)
    (assure distance-matrix
      (make 'distance-matrix
            :size size
            :content result))))


(-> make-distance-matrix-from-vector ((or list symbol) vector (-> (t t) single-float)) distance-matrix)
(defun make-distance-matrix-from-vector (type sequence function)
  (let* ((size (length sequence))
         (result (make-array (1+ (index-in-content-of-distance-matrix size
                                                                      (1- size)
                                                                      (1- size)))
                             :element-type type)))
    (labels ((fill-matrix (column)
               (iterate
                 (with first = (aref sequence column))
                 (for row from (1+ column) below size)
                 (for second = (aref sequence row))
                 (setf (aref result (index-in-content-of-distance-matrix size
                                                                         column
                                                                         row))
                       (funcall function first second)))))
      (iterate
        (for i from 0 below size)
        (fill-matrix i))
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
        ((or (>= from size) (>= to size))aref
         (error "No such position in the matrix."))
        ((eql from to)
         0.0)
        (t (let ((content (slot-value matrix '%content))
                 (from (min from to))
                 (to (max from to)))
             (declare (type simple-array content))
             (aref content (index-in-content-of-distance-matrix size
                                                                from
                                                                to))))))))


(defgeneric (setf distance) (value matrix from to)
  (:method (value (matrix distance-matrix) from to)
    (declare (type index from to)
             (optimize (speed 3)))
    (let ((size (slot-value matrix '%size)))
      (declare (type index size))
      (cond ((or (>= from size) (>= to size))
             (error "No such position in the matrix!"))
            ((eql from to)
             (error "Can't set dinstance to self"))
            (t (let ((content (slot-value matrix '%content))
                     (from (min from to))
                     (to (max from to)))
                 (declare (type simple-array content))
                 (setf
                  (aref content (index-in-content-of-distance-matrix size
                                                                     from
                                                                     to))
                  value)))))))


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
