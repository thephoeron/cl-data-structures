(in-package #:cl-data-structures.data-frame)


(defclass fundamental-data-frame ()
  ())


(nest
 (locally (declare (optimize (safety 3))))
 (defclass data-frame (fundamental-data-frame))
 ((%data :initarg :data
         :type cl-ds.seqs.rrb:transactional-rrb-vector
         :accessor access-data)
  (%dimensionality :initarg :dimensionality
                   :type (integer 2 *)
                   :accessor access-dimensionality
                   :reader cl-ds:dimensionality)
  (%sizes :initarg :sizes
          :reader read-sizes
          :type (vector non-negative-fixnum))
  (%aliases :initform (make-hash-table :test 'equal)
            :reader read-aliases
            :type hash-table)))


(nest
 (locally (declare (optimize (safety 3))))
 (defclass data-accessor ())
 ((%position :initarg :position
             :accessor access-position
             :type non-negative-fixnum
             :initform 0)
  (%frame :initarg :frame
          :reader read-frame
          :type data-frame)
  (%aliases :reader read-aliases
            :type hash-table
            :initarg :aliases)
  (%dimension :initarg :dimension
              :type non-negative-fixnum
              :reader read-dimension)
  (%data :initarg :data
         :reader read-data
         :type cl-ds.seqs.rrb:transactional-rrb-vector)))


(nest
 (locally (declare (optimize (safety 3))))
 (defclass fundamental-frame-dimension (cl-ds:fundamental-random-access-range))
 ((%dimension :initarg :dimension
              :reader read-dimension
              :type non-negative-fixnum)
  (%aliases :initarg :aliases
            :reader read-aliases
            :type hash-table)))


(nest
 (locally (declare (optimize (safety 3))))
 (defclass sequence-dimension (fundamental-frame-dimension))
 ((%range :initarg :range
          :reader read-range
          :type cl-ds:fundamental-random-access-range)))
