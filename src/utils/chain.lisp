(in-package #:cl-data-structures.utils)

#|
Simple and compact stack/queue
|#

(defun make-chain-link (&optional parent)
  (let ((result (make-array 16)))
    (setf (aref result 1) parent)
    result))


(-> chain-head (vector) t)
(defun chain-head (chain)
  (unless (zerop (aref chain 0))
    (aref chain 2)))


(-> chain-tail (vector) (or vector nil))
(defun chain-tail (chain)
  (aref chain 1))


(defun push-front (elt chain)
  (let ((size (aref chain 0)))
    (if (eql size 14)
        (let ((result (make-chain-link chain)))
          (setf (aref result 2) elt)
          result)
        (progn
          (setf (aref chain (+ 2 (incf (aref chain 0))))
                elt)
          chain))))
