(cl:in-package :cl-data-structures.utils)


(defun make-skip-vector (vector mask)
  (cons vector mask))


(defun make-new-skip-vector (size &key (element-type t) (mask (1- (ash 1 size))))
  (make-sparse-vector (make-array size :element-type element-type)
                      mask))


(declaim (inline start-position))
(defun start-position (index total-size skip-vector)
  (iterate
    (with start = 0)
    (with end = (ash total-size -1))
    (for count = (logcount (ldb (byte end start) (cdr skip-vector))))
    (cond ((< count (1+ index))
           (incf end (ash end -1)))
          ((> count (1+ index))
           (leave end))
          (t (leave end)))))


(declaim (inline skip-index))
(defun skip-index (index skip-vector)
  (let* ((total-size (array-dimension (car skip-vector) 0))
         (start-position (start-position index total-size skip-vector)))
    (iterate
      (with mask = (cdr skip-vector))
      (with result = (logcount (ldb (byte start-position 0) mask)))
      (for i from start-position below total-size)
      (until (eql result (1+ index)))
      (when (ldb-test (byte 1 i) mask)
        (incf result))
      (finally (return i)))))


(defun sref (skip-vector index)
  (aref (car skip-vector)
        (skip-index index skip-vector)))


(defun slength (skip-vector)
  (logcount (cdr skip-vector)))


(defun (setf sref) (new-value skip-vector index)
  (setf (aref (car skip-vector)
              (skip-index index skip-vector))
        new-value))


(defun skip-vector-without (skip-vector index)
  (make-sparse-vector (car skip-vector)
                      (dpb 0 (byte 1 index)
                           (cdr skip-vector))))


(defun to-dense-vector (skip-vector)
  (let* ((size (logcount (cdr skip-vector)))
         (type (array-element-type (car skip-vector)))
         (result (make-array size :element-type type)))
    (iterate
      (with j = 0)
      (for i from 0 below (length (car skip-vector)))
      (when (ldb-test (byte 1 i) (cdr skip-vector))
        (setf (aref result (finc j)) (aref (car skip-vector) i))))
    result))
