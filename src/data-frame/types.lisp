(in-package #:cl-data-structures.data-frame)


(defclass fundamental-data-frame ()
  ())


(locally (declare (optimize (safety 3)))
  (defclass data-frame (fundamental-data-frame)
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
     (%aliases :initform (make-hash-table)
               :reader read-aliases
               :type hash-table))))


(locally (declare (optimize (safety 3)))
  (defclass data-accessor ()
    ((%position :initarg :position
                :accessor access-position
                :type non-negative-fixnum
                :initform 0)
     (%frame :initarg :frame
             :reader read-frame
             :type data-frame)
     (%dimension :initarg :dimension
                 :type non-negative-fixnum
                 :reader read-dimension)
     (%data :initarg :data
            :reader read-data
            :type cl-ds.seqs.rrb:transactional-rrb-vector))))
