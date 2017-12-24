(in-package #:cl-data-structures.utils)

#|
Simple and compact stack/queue
|#

(define-constant +chain-size+ 31)
(define-constant +double-chain-size+ 29)


(defun make-chain-link (&optional next)
  (let ((result (make-array (+ 1 +chain-size+))))
    (setf (aref result 0) next)
    result))


(defun make-double-chain-link (&key prev next)
  (let ((result (make-array (+ 3 +double-chain-size+))))
    (setf (aref result 0) prev
          (aref result 1) next
          (aref result 2) 0)
    result))


(defun add-front (position link elt)
  (assert (< position (aref link 2)))
  (setf (aref link (+ 3 position)) elt)
  (incf (aref link 2))
  link)


(defun add-back (position link elt)
  (assert (>= (+ 2 (- +double-chain-size+ position)) (aref link 2)))
  (setf (aref link (+ 2 (- +double-chain-size+ position))) elt)
  link)


(defstruct chain-queue first last (last-count 0) (first-count 0) (pointer 0))


(defstruct double-chain-queue
  first last
  (last-count 0) (first-count 0)
  (first-pointer 0) (last-pointer 0))


(defun double-chain-queue-empty (queue)
  (with-slots (first) queue
    (null first)))


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


(defun double-chain-queue-put-back (elt queue)
  (with-slots (first last last-count first-count last-pointer first-pointer) queue
    (when (null last)
      (setf last (make-double-chain-link)))
    (when (null first)
      (setf first last
            first-pointer 0
            first-count last-count))
    (if (eql last-count +double-chain-size+)
        (let ((next (make-double-chain-link)))
          (setf last-count 1
                (aref last 1) next
                (aref next 0) last
                (aref next (+ 2 +double-chain-size+)) elt
                last-pointer 0
                last next))
        (progn
          (setf (aref last (+ 2 (- +double-chain-size+
                                   last-count
                                   (aref last 2))))
                elt)
          (incf last-count)))
    (when (null first)
      (setf first last))
    (when (eq first last)
      (setf first-count last-count)))
  queue)


(defun double-chain-queue-put-front (elt queue)
  (with-slots (first last last-count first-count last-pointer first-pointer) queue
    (when (null first)
      (setf first (make-double-chain-link)))
    (when (null last)
      (setf last first
            last-count first-count))
    (if (eql first-count +double-chain-size+)
        (let ((next (make-double-chain-link)))
          (setf first-count 1
                (aref first 0) next
                (aref next 1) first
                first next
                (aref next 3) elt
                (aref next 2) 1))
        (progn (setf (aref first (+ 3 (aref first 2))) elt)
               (incf first-count)
               (incf (aref first 2))))
    (when (null last)
      (setf last first))
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
              first-count (cond ((null first) 0)
                                ((eq first last) last-count)
                                (t +chain-size+))))
      (when (null first)
        (setf last nil
              last-count 0)))))


(defun chain-queue-on-each (fn queue)
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


(defun chain-queue-front (queue)
  (with-slots (first pointer) queue
    (unless (null first)
      (aref first (1+ pointer)))))
