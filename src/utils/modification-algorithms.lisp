(in-package :cl-ds.utils)


(-> pop-last (extendable-vector) t)
(defun pop-last (vector)
  "If fill pointer is larger than zero reduce fill pointer and @b(returns) last vector element.

 @b(Side effects:) reduce fill pointer of vector.

 @b(Exceptional situations:) will return nil if fill pointer is equal 0."
  (unless (zerop (fill-pointer vector))
    (prog1
        (aref vector (1- (fill-pointer vector)))
      (decf (fill-pointer vector)))))


(-> erase-from-vector (extendable-vector &rest index) vector)
(defun erase-from-vector (vector &rest indexes)
  "Remove elements under indexes from vector, preserving order of elements.

 @b(Side effects:) Reduces fill pointer. Removes elements from vector and shifts elements that are left."
  (let ((indexes (sort indexes #'<))
        (count 0))
    (iterate
      (for sub on indexes)
      (for next = (cadr sub))
      (for index = (car sub))
      (for start = (- index count))
      (for end = (and next (fill-pointer vector)))
      (replace vector vector
               :start1 start
               :end1 end
               :start2 (1+ start))
      (incf count)
      (finally (progn (decf (fill-pointer vector) count)
                      (return vector))))))


(-> swapop (extendable-vector index) vector)
(defun swapop (vector index)
  "Swaps element under INDEX with last element of VECTOR. Pops last element and returns VECTOR.

   @b(Values and parameters:)
   @begin(list)
   @item(vector -- input vector)
   @item(index -- under which index element will be removed?)
   @end(list)

   @b(Side Effects:) changes element under INDEX to last element in the vector, reduces fill-pointer."
  (unless (zerop (fill-pointer vector))
    (rotatef (aref vector (1- (fill-pointer vector)))
             (aref vector index))
    (decf (fill-pointer vector)))
  vector)
