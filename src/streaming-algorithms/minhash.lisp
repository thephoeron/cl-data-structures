(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass fundamental-minhash ()
  ((%k :initarg :k
       :reader read-k)))


(defclass minhash-corpus (fundamental-minhash)
  ((%table :initarg :table
           :reader read-table)))


(defclass callback-minhash (fundamental-minhash)
  ((%hash-function :initarg :hash-function
                   :reader read-hash-function)))


(defgeneric minhash-corpus-hash-value (corpus element))


(defmethod minhash-corpus-hash-value ((corpus minhash-corpus) element)
  (gethash element (read-table corpus)))


(defmethod minhash-corpus-hash-value ((corpus callback-minhash) element)
  (funcall (read-hash-function corpus) element))


(defmethod cl-ds:clone ((object minhash-corpus))
  (make (class-of object)
        :table (~> object read-table copy-hash-table)
        :function (read-function object)
        :k (read-k object)))


(cl-ds.utils:define-list-of-slots minhash-corpus ()
  (table read-table)
  (k read-k))


(cl-ds.alg.meta:define-aggregation-function gather-minhash-corpus
    gather-corpus-function
    (:range k &key key corpus)
    (:range k &key (key #'identity)
     (corpus (make-instance 'minhash-corpus
                            :k k
                            :table (make-hash-table :test 'equal))))
    (%table %k %corpus)
    ((check-type k positive-fixnum)
     (setf %corpus (cl-ds:clone corpus)
           %table (read-table %corpus)
           %k (read-k %corpus)))
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
       (finally (return %corpus)))))


(defun make-minhash (k &optional (hash-function #'sxhash))
  (make 'callback-minhash :k k :hash-function hash-function))


(-> minhash-corpus-minhash (minhash-corpus list) (simple-array fixnum (*)))
(defun minhash-corpus-minhash (corpus elements)
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


(defgeneric minhash (corpus elements))


(defmethod minhash :around ((corpus minhash-corpus) elements)
  (check-type corpus fundamental-minhash)
  (check-type elements list)
  (call-next-method corpus elements))


(defmethod minhash ((corpus minhash-corpus) elements)
  (minhash-corpus-minhash corpus elements))


(defmethod minhash ((corpus fundamental-minhash) elements)
  (iterate
    (declare (type fixnum count)
             (type (simple-array fixnum (*)) minis))
    (with count = (read-k corpus))
    (with minis = (make-array count :element-type 'fixnum
                                    :initial-element most-positive-fixnum))
    (for element in elements)
    (for sub = (minhash-corpus-hash-value corpus element))
    (when (null sub)
      (next-iteration))
    (iterate
      (declare (type fixnum i))
      (for i from 0 below count)
      (minf (aref minis i) (aref (the (simple-array fixnum (*)) sub)
                                 i)))
    (finally (return minis))))


(-> minhash-jaccard/fixnum ((simple-array fixnum (*)) (simple-array fixnum (*)))
    non-negative-fixnum)
(defun minhash-jaccard/fixnum (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-ds.utils:lolol (a b)
    (check-type a (simple-array fixnum (*)))
    (check-type b (simple-array fixnum (*)))
    (unless (= (length a) (length b))
      (error 'cl-ds:incompatible-arguments
             :parameters '(a b)
             :values (list a b)
             :format-control "Lengths of input vectors must be equal."))
    (iterate
      (declare (type fixnum i len result))
      (with len = (length a))
      (with result = len)
      (for i from 0 below len)
      (when (= (aref a i) (aref b i))
        (decf result))
      (finally (return result)))))


(-> minhash-jaccard/single-float ((simple-array fixnum (*)) (simple-array fixnum (*)))
    single-float)
(defun minhash-jaccard/single-float (a b)
  (declare (optimize (speed 3)))
  (let ((result (minhash-jaccard/fixnum a b))
        (length (length a)))
    (declare (type fixnum result length))
    (/ (coerce result 'single-float) length)))


(-> minhash-jaccard/double-float ((simple-array fixnum (*)) (simple-array fixnum (*)))
    double-float)
(defun minhash-jaccard/double-float (a b)
  (declare (optimize (speed 3)))
  (let ((result (minhash-jaccard/fixnum a b))
        (length (length a)))
    (declare (type fixnum result length))
    (/ (coerce result 'double-float) length)))
