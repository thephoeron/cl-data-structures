(in-package #:cl-ds.utils)


(defun ordered-p (sequence fn &key (key #'identity))
  "Predictate. Checks if SEQUENCE is ordered by FN (that is, if call to (fn prev current) always returns T).

  @b(Side Effects:) none. "
  (iterate
    (for s in-sequence sequence)
    (for elt = (funcall key s))
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


(-> lower-bound (vector t (-> (t t) boolean)
                        &key (:key function) (:start non-negative-fixnum) (:end non-negative-fixnum))
    index)
(defun lower-bound (vector element comparsion &key (key #'identity) (start 0) (end (length vector)))
  (declare (optimize (speed 3)))
  (iterate
    (for current = (truncate (/ (+ end start) 2)))
    (until (eql end start))
    (if (funcall comparsion
                 (funcall key (aref vector current))
                 element)
        (setf start (1+ current))
        (setf end current))
    (finally (return current))))


(defun on-ordered-intersection (function first-order second-order
                                &key
                                  (on-first-missing #'identity)
                                  (on-second-missing #'identity)
                                  (key #'identity)
                                  (same #'=)
                                  (less #'<))
  (with-vectors (first-order second-order)
    (iterate
      (with a = 0)
      (with b = 0)
      (while (< a (length first-order)))
      (while (< b (length second-order)))
      (for av = (funcall key (first-order a)))
      (for bv = (funcall key (second-order b)))
      (cond ((funcall same av bv)
             (progn
               (funcall function (first-order a) (second-order b))
               (incf a)
               (incf b)))
            ((funcall less av bv)
             (progn
               (funcall on-first-missing (first-order a))
               (incf a)))
            (t
             (progn
               (funcall on-second-missing (second-order b))
               (incf b))))
      (finally
       (iterate
         (for i from a below (length first-order))
         (funcall on-first-missing (first-order i)))
       (iterate
         (for i from b below (length second-order))
         (funcall on-second-missing (second-order i)))))))


(defun ordered-intersection (compare-fn test-fn vector &rest more-vectors)
  (push vector more-vectors)
  (iterate
    (with indexes = (map '(vector fixnum) (constantly 0) more-vectors))
    (with sizes = (map '(vector fixnum) #'length more-vectors))
    (with vector-type = (iterate
                          (for vector in (rest more-vectors))
                          (for prev-vector
                               previous vector
                               initially (first more-vectors))
                          (unless (equal (array-element-type vector)
                                         (array-element-type prev-vector))
                            (leave t))
                          (finally (return (array-element-type prev-vector)))))
    (with result = (make-array (reduce #'min more-vectors :key #'length)
                               :element-type vector-type
                               :adjustable t
                               :fill-pointer 0))
    (while (every #'< indexes sizes))
    (iterate
      (with ok = t)
      (with minimum = nil)
      (with minimum-i = 0)
      (for i from 0)
      (for index in-vector indexes)
      (for vector in more-vectors)
      (for value = (aref vector index))
      (for prev-value previous value)
      (if (first-iteration-p)
          (setf minimum value)
          (progn
            (when ok
              (setf ok (funcall test-fn value prev-value)))
            (when (and (not ok) (funcall compare-fn value minimum))
              (setf minimum value
                    minimum-i i))))
      (finally (if ok
                   (progn
                     (map-into indexes #'1+ indexes)
                     (vector-push-extend minimum result))
                   (incf (aref indexes minimum-i)))))
    (finally (return (adjust-array result
                                   (fill-pointer result))))))


(defun ordered-exclusion (compare-fn test-fn vector &rest more-vectors)
  (iterate
    (with indexes = (map '(vector fixnum) (constantly 0) more-vectors))
    (with vector-type = (iterate
                          (for vector in (rest more-vectors))
                          (for prev-vector
                               previous vector
                               initially (first more-vectors))
                          (unless (equal (array-element-type vector)
                                         (array-element-type prev-vector))
                            (leave t))
                          (finally (return (array-element-type prev-vector)))))
    (with result = (make-array (reduce #'min more-vectors :key #'length)
                               :element-type vector-type
                               :adjustable t
                               :fill-pointer 0))
    (for v in-vector vector)
    (map-into indexes
              (lambda (vector) (lower-bound vector v compare-fn))
              more-vectors)
    (for not-present =
         (iterate
           (for other-vector in more-vectors)
           (for index in-vector indexes)
           (always (or (eql index (length other-vector))
                       (let ((data (aref other-vector index)))
                         (not (funcall test-fn data v)))))))
    (when not-present
      (vector-push-extend v result))
    (finally (return (adjust-array result
                                   (fill-pointer result))))))


(defun revert-block (vector start end)
  (iterate
    (for i from start below end)
    (for j from (1- end) downto start)
    (while (< i j))
    (rotatef (aref vector i) (aref vector j)))
  vector)


(defun cyclic-shift (vector start end offset)
  (check-type vector vector)
  (check-type offset fixnum)
  (if (eql offset 1)
      (iterate
        (with tmp = (aref vector (1- end)))
        (for i from start below end)
        (rotatef (aref vector i) tmp))
      (progn
        (revert-block vector start end)
        (revert-block vector start offset)
        (revert-block vector offset end)))
  vector)


(defun shift-right (vector start new-start end)
  (cyclic-shift vector start end (- end start new-start)))


;; (defun merge-sort (vector predicate &key (key #'identity) (start 0) (end (length vector)))
;;   (when (< start end)
;;     (iterate
;;       (for block-size initially 2 then (ash block-size 1))
;;       (while (< block-size (- end start)))
;;       (iterate
;;         (for i from 0 by block-size)
;;         (for block-end = (min end (+ i block-size)))
;;         (for block-center = (truncate (/ (+ i block-end) 2)))
;;         (merge-in-place vector predicate key i block-center block-end)
;;         (until (eql block-end end)))))
;;   vector


(defun largest-power-of-3 (size)
  (declare (type fixnum size)
           (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (do ((i 1 (the fixnum (* i 3))))
      ((>= (the fixnum (* i 3)) size) i)))


(defun next-index (j len &aux (m (1+ len)))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum j len m))
  (do ((k (the fixnum (ash j 1)) (the fixnum (- k m))))
      ((< k m) k)))


(defun rotate-cycle-leader (vector start leader section-offset section-len)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0))
           (type vector vector)
           (type fixnum start leader section-offset section-len))
  (do ((i (next-index leader section-len)
          (next-index i section-len)))
      ((eql i leader) vector)
    (let ((start+section-offset (+ start section-offset)))
      (declare (type fixnum start+section-offset))
      (rotatef (aref vector (the fixnum (1- (the fixnum (+ start+section-offset
                                                           (the fixnum i))))))
               (aref vector (the fixnum (1- (the fixnum (+ start+section-offset
                                                           leader)))))))))


(defun inverse-faro-shuffle (vector start end)
  (iterate
    (with m = 0)
    (with n = 1)
    (with position = start)
    (while (< m n))
    (for section-len = (- end position))
    (for h = (largest-power-of-3 section-len))
    (setf m (if (> h 1) (ash h -1) 1)
          n (ash section-len -1))
    (for 2m = (ash m 1))
    (unless (eql n m)
      (cyclic-shift vector (+ position m) (+ position n m) 2m))
    (iterate
      (for leader initially 1 then (* leader 3))
      (while (< leader 2m))
      (rotate-cycle-leader vector
                           start
                           leader
                           (- position start)
                           2m))
    (incf position 2m))
  vector)


(defun faro-shuffle (vector start end)
  (assert (evenp (- end start)))
  (iterate
    (with m = 0)
    (with n = 1)
    (with position = start)
    (while (< m n))
    (for section-len = (- end position))
    (for h = (largest-power-of-3 section-len))
    (setf m (if (> h 1) (ash h -1) 1)
          n (ash section-len -1))
    (for 2m = (ash m 1))
    (unless (eql n m)
      (cyclic-shift vector (+ position m) (+ position n m) 2m))
    (iterate
      (for leader initially 1 then (* leader 3))
      (while (< leader 2m))
      (rotate-cycle-leader vector
                           start
                           leader
                           (- position start)
                           2m))
    (incf position 2m))
  vector)


(print (inverse-faro-shuffle "a1b2c3d4e5f6g7" 0 14))
;; (print (inverse-faro-shuffle (faro-shuffle #(1 2 3 4 5 6 7 8 9 10) 0 10) 0 10))
(print (faro-shuffle #(1 2 3 4 5 6 7 8 9 10) 0 10))
;; (print (shift-right #(0 1 2 3 4 5 6 7 8 9 10 11 12 13) 0 4 14))
