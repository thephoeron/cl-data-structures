(in-package #:cl-ds.utils)


(defun ordered-p (sequence fn)
  "Predictate. Checks if SEQUENCE is ordered by FN (that is, if call to (fn prev current) always returns T).

  @b(Side Effects:) none. "
  (iterate
    (for elt in-sequence sequence)
    (for pelt previous elt)
    (if-first-time
     t
     (always (funcall fn pelt elt))))) ()


(-> merge-ordered-vectors (symbol (-> (t t) boolean) vector &rest vector) vector)
(defun merge-ordered-vectors (result-type compare-fn vector &rest vectors)
  "Take few ordered vectors (by function compare-fn) and create new vector of element-type RESULT-TYPE by copying values from vectors in such way that it is also ordered. Return new vector.

 @b(Arguments and values:)
 @begin(list)
 @item(result-type -- element-type of result vector)
 @item(compare-fn -- function that describes ordering of vectors)
 @item(vector) -- ordered vector that is about to be merged
 @item(vectors -- more ordered vectors that are about to be merged)
 @end(list)

 @b(Side effects:) none."
  (let* ((vectors (cons vector vectors))
         (result (make-array 0
                             :adjustable t
                             :element-type result-type
                             :fill-pointer 0))
         (index.vector (make-array 0
                                   :element-type 'list
                                   :adjustable t
                                   :fill-pointer 0)))
    (map nil
         (lambda (x) (unless (zerop (array-dimension x 0))
                       (vector-push-extend (list* 0 x)
                                           index.vector)))
         vectors)
    (iterate
      (for item = (iterate
                    (for current index-of-vector index.vector)
                    (for (index . vector) = (aref index.vector current))
                    (for value = (aref vector index))
                    (for minimum first (list* value current)
                         then (if (funcall compare-fn value (car minimum))
                                  (list* value current)
                                  minimum))
                    (finally (return minimum))))
      (while item)
      (for (minimum . index) = item)
      (vector-push-extend minimum result)
      (for current = (aref index.vector index))
      (when  (= (incf (car current))
                (array-dimension (cdr current)
                                 0))
        (swapop index.vector index)))
    result))


(declaim (inline lower-bound))
(-> lower-bound (vector t (-> (t t) boolean)) index)
(defun lower-bound (vector element comparsion)
  (declare (optimize (speed 3)))
  (let ((length (length vector)))
    (iterate
      (with end = length)
      (with start = 0)
      (for current = (truncate (/ (+ end start) 2)))
      (until (eql end start))
      (if (funcall comparsion
                   (aref vector current)
                   element)
          (setf start (1+ current))
          (setf end current))
      (finally (return current)))))


(defun on-ordered-intersection (function first-order second-order
                                &key
                                  (on-first-missing #'identity)
                                  (on-second-missing #'identity)
                                  (key #'identity))
  (with-vectors (first-order second-order)
    (iterate
      (with a = 0)
      (with b = 0)
      (while (< a (length first-order)))
      (while (< b (length second-order)))
      (for av = (funcall key (first-order a)))
      (for bv = (funcall key (second-order b)))
      (cond ((eql av bv)
             (progn
               (funcall function a b)
               (incf a)
               (incf b)))
            ((< av bv)
             (progn
               (funcall on-first-missing a)
               (incf a)))
            ((< bv av)
             (progn
               (funcall on-second-missing b)
               (incf b))))
      (finally
       (iterate
         (for i from a below (length first-order))
         (funcall on-first-missing i))
       (iterate
         (for i from b below (length second-order))
         (funcall on-second-missing i))))))
