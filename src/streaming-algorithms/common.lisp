(cl:in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)
(define-constant +max-64-bit+ #XFFFFFFFFFFFFFFFF)


(-> hashval-no-depth ((simple-array (unsigned-byte 64) (* 2))
                      fixnum
                      (unsigned-byte 64))
    (unsigned-byte 64))
(declaim (inline hashval-no-depth))
(defun hashval-no-depth (hashes j hash)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 64) (* 2)) hashes)
           (type (unsigned-byte 64) hash)
           (type fixnum j))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 64 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 64 0) _)))


(defun hashval (hashes depth j hash)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 64) (* 2)) hashes)
           (type non-negative-fixnum depth j hash))
  (~> (hashval-no-depth hashes j hash)
      (rem depth)))


(defun make-hash-array (count)
  (lret ((result (make-array (list count 2) :element-type '(unsigned-byte 64))))
    (map-into (cl-ds.utils:unfold-table result)
              (lambda ()
                (~> (random +max-64-bit+)
                    (* +long-prime+)
                    (ldb (byte 64 0) _))))))


(defclass fundamental-data-sketch ()
  ((%hash-fn :initarg :hash-fn
             :accessor access-hash-fn)))


(defmethod cl-ds.utils:cloning-information append
    ((sketch fundamental-data-sketch))
  '((:hash-fn access-hash-fn)))


(defmethod initialize-instance :after ((sketch fundamental-data-sketch)
                                       &rest all)
  (declare (ignore all))
  (ensure-functionf (access-hash-fn sketch)))


(defgeneric compatible-p (first-sketch &rest more-sketches)
  (:method :around ((a fundamental-data-sketch) &rest more-sketches)
    (unless (every (curry #'eq (class-of a))
                   (mapcar #'class-of more-sketches))
      (return-from compatible-p nil))
    (unless (every (curry #'eq (access-hash-fn a))
                   (mapcar #'access-hash-fn more-sketches))
      (warn "Hashing function objects in the sketches mismatches. This may be a problem…"))
    (call-next-method)))


(defgeneric clean-sketch (function &rest arguments &key))


(defgeneric union (first-sketch &rest more-sketches)
  (:method :around ((sketch fundamental-data-sketch) &rest more-sketches)
    (unless (apply #'compatible-p sketch more-sketches)
      (error 'cl-ds:incompatible-arguments
             :parameters '(sketch more-sketches)
             :values `(,sketch ,more-sketches)
             :format-control "Sketches passed to the union are not compatible."))
    (call-next-method)))
