(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass minhash-corpus ()
  ((%table :initarg :table
           :reader read-table)
   (%k :initarg :k
       :reader read-k)))


(cl-ds.utils:define-list-of-slots minhash-corpus ()
  (table read-table)
  (k read-k))


(cl-ds.alg.meta:define-aggregation-function gather-minhash-corpus
    gather-corpus-function
    (:range k &key key)
    (:range k &key (key #'identity))
    (%table %k)
    ((&key k &allow-other-keys)
     (check-type k positive-fixnum)
     (setf %table (make-hash-table :test 'equal)
           %k k))
    ((element)
     (ensure (gethash element %table) (make-array %k :element-type 'fixnum)))
    ((iterate
       (with corpus-size = (hash-table-count %table))
       (with input-list = (~>> %table hash-table-alist))
       (with pointers = (make-hash-table :test 'eq))
       (repeat %k)
       (iterate
         (for i from 0 below corpus-size)
         (for (key . value) in (shuffle input-list))
         (for pointer = (gethash value pointers 0))
         (setf (aref value pointer) i)
         (incf (gethash value pointers 0)))
       (finally (return (make-instance 'minhash-corpus
                                       :k %k
                                       :table %table))))))


(-> minhash* (minhash-corpus list) (simple-array fixnum (*)))
(defun minhash* (corpus elements)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-ds.utils:with-slots-for (corpus minhash-corpus)
    (iterate
      (declare (type fixnum count)
               (type (simple-array fixnum (*)) minis)
               (type hash-table hash-table))
      (with hash-table = table)
      (with count = k)
      (with minis = (make-array count :element-type 'fixnum
                                      :initial-element most-positive-fixnum))
      (for element in elements)
      (for sub = (gethash element hash-table))
      (when (null sub)
        (next-iteration))
      (iterate
        (declare (type fixnum i))
        (for i from 0 below count)
        (minf (aref minis i) (aref (the (simple-array fixnum (*)) sub)
                                   i)))
      (finally (return minis)))))


(defun minhash (corpus elements)
  (check-type elements list)
  (check-type corpus minhash-corpus)
  (minhash* corpus elements))
