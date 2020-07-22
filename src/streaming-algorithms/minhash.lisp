(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass fundamental-minhash ()
  ((%k :initarg :k
       :reader read-k)))


(defclass minhash-corpus (fundamental-minhash)
  ((%table :initarg :table
           :reader read-table)))


(defclass callback-minhash (fundamental-minhash)
  ((%hash-function :initarg :hash-function
                   :reader read-hash-function)
   (%hash-array :initarg :hash-array
                 :reader read-hash-array)))


(defgeneric minhash-corpus-hash-value (corpus element))


(defmethod minhash-corpus-hash-value ((corpus minhash-corpus) element)
  (gethash element (read-table corpus)))


(defmethod minhash-corpus-hash-value ((corpus callback-minhash) element)
  (let* ((k (read-k corpus))
         (hash-array (read-hash-array corpus))
         (result (make-array (read-k corpus) :element-type '(unsigned-byte 64)))
         (hash (funcall (read-hash-function corpus) element)))
    (iterate
      (for i from 0 below k)
      (setf (aref result i) (hashval-no-depth hash-array i hash))
      (finally (return result)))))


(defmethod cl-ds:clone ((object minhash-corpus))
  (make (class-of object)
        :table (~> object read-table copy-hash-table)
        :k (read-k object)))


(defmethod cl-ds:clone ((object callback-minhash))
  (make (class-of object)
        :hash-function (read-hash-function object)
        :hash-array (read-hash-array object)))


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
     (ensure (gethash element %table) (make-array %k :element-type '(unsigned-byte 64))))
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


(defun make-minhash (k &key
                         (hash-function #'sxhash)
                         (hash-array (make-hash-array k)))
  (make 'callback-minhash :k k :hash-function hash-function
        :hash-array hash-array))


(-> minhash-corpus-minhash (minhash-corpus list) (simple-array (unsigned-byte 64) (*)))
(defun minhash-corpus-minhash (corpus elements)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-ds.utils:with-slots-for (corpus minhash-corpus)
    (bind ((count (the fixnum (read-k corpus)))
           (hash-table table)
           (minis (make-array count :element-type '(unsigned-byte 64)
                                  :initial-element most-positive-fixnum))
         ((:flet impl (element))
          (let ((sub (gethash element hash-table)))
            (unless (null sub)
              (iterate
                (declare (type fixnum i))
                (for i from 0 below count)
                (minf (aref minis i) (aref (the (simple-array (unsigned-byte 64) (*)) sub)
                                           i)))))))
    (cl-ds:across elements #'impl)
    minis)))


(defgeneric minhash (corpus elements))


(defmethod minhash ((corpus minhash-corpus) elements)
  (minhash-corpus-minhash corpus elements))


(defmethod minhash ((corpus fundamental-minhash) elements)
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (bind ((count (the fixnum (read-k corpus)))
         (minis (make-array count :element-type '(unsigned-byte 64)
                                  :initial-element most-positive-fixnum))
         ((:flet impl (element))
          (let ((sub (minhash-corpus-hash-value corpus element)))
            (declare (type (simple-array (unsigned-byte 64) (*)) sub))
            (unless (null sub)
              (iterate
                (declare (type fixnum i))
                (for i from 0 below count)
                (minf (aref minis i) (aref (the (simple-array (unsigned-byte 64) (*)) sub)
                                           i)))))))
    (cl-ds:across elements #'impl)
    minis))


(-> minhash-jaccard/fixnum ((simple-array (unsigned-byte 64) (*)) (simple-array (unsigned-byte 64) (*)))
    fixnum)
(defun minhash-jaccard/fixnum (a b)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cl-ds.utils:lolol (a b)
    (check-type a (simple-array (unsigned-byte 64) (*)))
    (check-type b (simple-array (unsigned-byte 64) (*)))
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


(-> minhash-jaccard/single-float ((simple-array (unsigned-byte 64) (*)) (simple-array (unsigned-byte 64) (*)))
    single-float)
(defun minhash-jaccard/single-float (a b)
  (declare (optimize (speed 3)))
  (let ((result (minhash-jaccard/fixnum a b))
        (length (length a)))
    (declare (type fixnum result length))
    (/ (coerce result 'single-float) length)))


(-> minhash-jaccard/double-float ((simple-array (unsigned-byte 64) (*)) (simple-array (unsigned-byte 64) (*)))
    double-float)
(defun minhash-jaccard/double-float (a b)
  (declare (optimize (speed 3)))
  (let ((result (minhash-jaccard/fixnum a b))
        (length (length a)))
    (declare (type fixnum result length))
    (/ (coerce result 'double-float) length)))
