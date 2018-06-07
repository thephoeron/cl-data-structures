(in-package #:cl-data-structures.data-frame)


(locally (declare (optimize (safety 3)))
  (defclass data-frame ()
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
