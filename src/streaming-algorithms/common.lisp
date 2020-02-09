(cl:in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)


(defun hashval (hashes depth j hash)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum (* 2)) hashes)
           (type non-negative-fixnum depth j hash))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 32 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 32 0) _)
      (rem +long-prime+)
      (rem depth)))


(defun make-hash-array (count)
  (lret ((result (make-array (list count 2) :element-type 'non-negative-fixnum)))
    (map-into (cl-ds.utils:unfold-table result)
              (curry #'random most-positive-fixnum))))


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
                   more-sketches)
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
