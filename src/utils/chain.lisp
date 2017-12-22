(in-package #:cl-data-structures.utils)

#|
Simple and compact stack/queue
|#

(define-constant +chain-size+ 31)


(defun make-chain-link (&optional next)
  (let ((result (make-array (+ 1 +chain-size+))))
    (setf (aref result 0) next)
    result))


(defstruct chain-queue first last (last-count 0) (first-count 0) (pointer 0))


(defun chain-queue-empty (queue)
  (with-slots (first) queue
    (null first)))


(defun chain-queue-put (elt queue)
  (with-slots (first last last-count first-count) queue
    (when (null last)
      (setf last (make-chain-link)))
    (when (null first)
      (setf first last
            first-count last-count))
    (if (eql last-count +chain-size+)
        (let ((next (make-chain-link)))
          (setf last-count 1
                (aref last 0) next
                (aref next 1) elt
                last next))
        (setf (aref last (incf last-count)) elt))
    (when (null first)
      (setf first last))
    (when (eq first last)
      (setf first-count last-count)))
  queue)


(defun chain-queue-take (queue)
  (with-slots (first last first-count last-count pointer) queue
    (prog1 (aref first (1+ pointer))
      (incf pointer)
      (when (eql pointer first-count)
        (setf first (aref first 0)
              pointer 0
              first-count (cond ((null first) 0) ((eq first last) last-count) (t +chain-size+))))
      (when (null first)
        (setf last nil
              last-count 0)))))


(defun chain-on-each (fn queue)
  (declare (optimize (debug 3)))
  (with-slots (first first-count last-count pointer) queue
    (iterate
      (for link
           initially first
           then (aref link 0))
      (until (null link))
      (iterate
        (for i from 1)
        (repeat (cond ((eq first link) first-count)
                      ((null (aref link 0)) last-count)
                      (t +chain-size+)))
        (funcall fn (aref link i))))
    queue))
