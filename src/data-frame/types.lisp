(in-package #:cl-data-structures.data-frame)


(nest
 (locally (declare (optimize (safety 3))))
 (defclass fundamental-data-frame ())
 ((%aliases :initform (make-hash-table :test 'equal)
            :reader read-aliases
            :initarg :aliases
            :type hash-table)
  (%reverse-alias :initarg :reverse-alias
                  :reader read-reverse-aliases
                  :type hash-table
                  :initform (make-hash-table :test 'equal))))


(nest
 (locally (declare (optimize (safety 3))))
 (defclass data-frame (fundamental-data-frame))
 ((%data :initarg :data
         :type cl-ds.seqs.rrb:mutable-rrb-vector
         :accessor access-data)
  (%dimensionality :initarg :dimensionality
                   :type (integer 2 *)
                   :accessor access-dimensionality
                   :reader cl-ds:dimensionality)
  (%lower-bounds :initarg :lower-bounds
                 :reader read-lower-bounds
                 :type (vector non-negative-fixnum))
  (%upper-bounds :initarg :upper-bounds
                 :reader read-upper-bounds
                 :type (vector non-negative-fixnum))))


(nest
 (locally (declare (optimize (safety 3))))
 (defclass proxy-data-frame (fundamental-data-frame))
 ((%inner-data-frame :initarg :inner-data-frame
                     :reader read-inner-data-frame)
  (%dimensionality :initarg :dimensionality
                   :reader read-dimensionality
                   :reader cl-ds:dimensionality)
  (%pinned-axis :initarg :pinned-axis
                :type list
                :reader read-pinned-axis)))


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


(defun make-data-accessor (data instance dimension)
  (make 'data-accessor
        :frame instance
        :aliases (read-aliases instance)
        :dimension dimension
        :data data))
