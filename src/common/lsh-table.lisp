(in-package #:cl-user)


(defpackage :cl-data-structures.common.lsh
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.common.lsh)
  (:export))


(in-package #:cl-data-structures.common.lsh)


(-> vector-dot-product ((vector single-float) (vector single-float))
    single-float)
(defun vector-dot-product (a b)
  (declare (type (vector single-float) a b)
           (optimize (speed 3)))
  (let ((sum 0.0))
    (declare (type single-float sum))
    (map nil
         (lambda (ea eb)
           (declare (type single-float ea eb))
           (incf sum (* ea eb)))
         a b)
    sum))


(-> project-point ((vector single-float)
                   (vector single-float)
                   (vector single-float))
    (vector single-float))
(defun project-point (a b p)
  (declare (type (vector single-float) a b p)
           (optimize (speed 3)))
  (bind ((ap (map '(vector single-float) #'- p a))
         (ab (map '(vector single-float) #'- b a))
         (dot-ap-ab (vector-dot-product ap ab))
         (dot-ab-ab (vector-dot-product ab ab))
         (ratio (/ dot-ap-ab dot-ab-ab))
         (result (map '(vector single-float)
                      (lambda (a ab)
                        (declare (type single-float a ab))
                        (+ a (* ab ratio)))
                      a
                      ab)))
    result))


(-> distance ((vector single-float) (vector single-float))
    single-float)
(defun distance (point1 point2)
  (declare (type (vector single-float) point1 point2)
           (optimize (speed 3)))
  (let ((sum 0.0))
    (declare (type (single-float 0.0) sum))
    (map nil
         (lambda (a b &aux (diff (- a b)))
           (declare (type single-float diff a b))
           (incf sum (expt diff 2)))
         point1
         point2)
    (sqrt sum)))


(-> bucket (single-float single-float) integer)
(defun bucket (distance span)
  (declare (type single-float distance span)
           (optimize (speed 3)))
  (floor distance span))


(defstruct hash-function-args
  (point-1 (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (point-2 (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  (span 0.0 :type single-float))


(defun projection-bucket (args vector)
  (declare (type (vector single-float) vector)
           (type hash-function-args args))
  (let ((point-1 (hash-function-args-point-1 args))
        (point-2 (hash-function-args-point-2 args))
        (span (hash-function-args-span args)))
    (~> (project-point point-1 point-2 vector)
        (distance point-1)
        (bucket span))))


(defstruct table
  (args (make-hash-function-args) :type hash-function-args)
  (hash-table (make-hash-table) :type hash-table))


(defun insert (vector element table)
  (declare (type table table))
  (let* ((bucket (~> table table-args (projection-bucket vector)))
         (hash-table (table-hash-table table))
         (content #1=(gethash bucket hash-table)))
    (if (null content)
        (setf #1# (vect element))
        (vector-push-extend element content))
    element))


(defun find-bucket (vector table)
  (~> table table-args (projection-bucket vector)
      (gethash (table-hash-table table))))


(defclass euclid-distance-lsh-table ()
  ((%tables :initarg :tables
            :type vector
            :reader read-tables
            :initform (vect))
   (%key :initarg :key
         :type function
         :reader read-key
         :initform #'identity)
   (%vector-length :initarg :vector-length
                   :type fixnum
                   :reader read-vector-length)))


(defun insert-into-euclid-distance-lsh-table (table element)
  (let* ((key (read-key table))
         (vector (funcall key element))
         (vector-length (read-vector-length table)))
    (check-type vector (vector single-float))
    (unless (= (length vector) vector-length)
      (error 'cl-ds:dimensionality-error
             :bounds vector-length
             :text "Vector has invalid length."
             :value (length vector)))
    (map nil (curry #'insert vector element) (read-tables table))))


(defun euclid-distance-lsh-table-find-buckets (table element)
  (let* ((key (read-key table))
         (vector (funcall key element))
         (vector-length (read-vector-length table)))
    (check-type vector (vector single-float))
    (unless (= (length vector) vector-length)
      (error 'cl-ds:dimensionality-error
             :bounds vector-length
             :text "Vector has invalid length."
             :value (length vector)))
    (~>> (read-tables table)
         (map 'vector (curry #'find-bucket vector))
         (delete-if #'null))))
