(in-package #:cl-data-structures.utils)

#|
Simple and compact stack/queue
|#

(define-constant +chain-size+ 14)


(defun make-chain-link (&optional next)
  (let ((result (make-array (+ 2 +chain-size+))))
    (setf (aref result 1) next
          (aref result 0) 0)
    result))


(-> chain-head (vector) t)
(defun chain-front (chain)
  (unless (null chain)
    (let ((size (aref chain 0)))
      (unless (zerop (aref chain 0))
        (aref chain (+ 1 size))))))


(defmacro chain-push-front (elt first-chain-link)
  (once-only (elt)
    (with-gensyms (!size !result)
      `(let ((,!size (if (null ,first-chain-link)
                         +chain-size+
                         (aref ,first-chain-link 0))))
        (if (eql ,!size +chain-size+)
            (let ((,!result (make-chain-link ,first-chain-link)))
              (setf (aref ,!result 2) ,elt
                    ,first-chain-link ,!result))
            (progn
              (setf (aref ,first-chain-link
                          (+ 2 (incf (aref ,first-chain-link 0))))
                    ,elt)
              ,first-chain-link))))))


(defmacro chain-push-back (elt last-chain-link)
  (once-only (elt)
    (with-gensyms (!size !result)
      `(if (null ,last-chain-link)
           (let ((,!result (make-chain-link)))
             (setf (aref ,!result 0) 1
                   (aref ,!result 2) ,elt
                   ,last-chain-link ,!result)
             ,!result)
           (let ((,!size (aref ,last-chain-link 0)))
             (if (eql ,!size +chain-size+)
                 (let ((,!result (make-chain-link)))
                   (setf (aref ,!result 2) ,elt
                         (aref ,!result 0) 1
                         (aref ,last-chain-link 1) ,!result
                         ,last-chain-link ,!result)
                   ,!result)
                 (progn
                   (incf (aref ,last-chain-link 0))
                   (shiftf ,@(mapcar (lambda (x) `(aref ,last-chain-link ,(+ 2 x)))
                                     (reverse (iota +chain-size+)))
                           ,elt)
                   ,last-chain-link)))))))


(defmacro chain-pop-front (chain)
  (with-gensyms (!front)
    `(unless (null ,chain)
       (let ((,!front (chain-front ,chain)))
         (when (zerop (decf (aref ,chain 0)))
           (setf ,chain (aref ,chain 1)))
         ,!front))))


(defstruct chain-queue first last)


(defun chain-queue-empty (queue)
  (with-slots (first) queue
    (null first)))


(defun chain-queue-put (elt queue)
  (with-slots (first last) queue
    (when (null first)
      (setf first last))
    (chain-push-back elt last))
  queue)


(defun chain-queue-take (queue)
  (with-slots (first last) queue
    (prog1 (chain-pop-front first)
      (when (null first)
        (setf last nil)))))
