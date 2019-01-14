(in-package #:cl-data-structures.utils)


(defun highest-leader (length)
  (declare (type fixnum length)
           (optimize (speed 3)))
  (iterate
    (declare (type non-negative-fixnum i p-i))
    (for i initially 1
         then (the fixnum (* i 3)))
    (for p-i previous i initially 1)
    (unless (< i length)
      (leave p-i))))


(defun next-index (j length)
  (declare (type fixnum j length)
           (optimize (speed 3)))
  (setf j (* j 2))
  (let ((m (1+ length)))
    (declare (type fixnum m))
    (iterate
      (while (>= j m))
      (decf j m))
    j))


(defun cycle-rotate (array leader
                     section-offset
                     section-length)
  (declare (optimize (speed 3))
           (type vector array)
           (type fixnum leader section-offset section-length))
  (iterate
    (declare (type fixnum i i-1 leader-1))
    (for i initially (next-index leader section-length)
         then (next-index i section-length))
    (until (eql i leader))
    (for i-1 = (1- i))
    (for leader-1 = (1- leader))
    (rotatef (aref array (the fixnum (+ section-offset i-1)))
             (aref array (the fixnum (+ section-offset leader-1))))))


(defun circular-shift-left (array start end shift)
  (declare (optimize (speed 3))
           (type vector array)
           (type fixnum start end shift))
  (let* ((length (- end start))
         (shift (rem shift length)))
    (declare (type fixnum length shift))
    (unless (zerop shift)
      (iterate
        (declare (type fixnum i j gcd))
        (with gcd = (gcd length shift))
        (for i from 0 below gcd)
        (for tmp = (aref array (+ i start)))
        (for j = i)
        (iterate
          (declare (type fixnum k))
          (for k = (+ j shift))
          (when (>= k length)
            (decf k length))
          (when (eql k i)
            (leave))
          (setf (aref array (the fixnum (+ j start)))
                (aref array (the fixnum (+ k start)))
                j k))
        (setf (aref array (+ j start)) tmp))))
  array)


(defun circular-shift-right (array start end shift)
  (declare (type fixnum shift)
           (type non-negative-fixnum start end)
           (optimize (debug 3)))
  (let ((length (- end start)))
    (declare (type non-negative-fixnum length))
    (circular-shift-left array start end
                         (- length (rem shift length)))))


(defun circular-shift (array start end shift)
  (declare (optimize (speed 3))
           (type fixnum start end shift)
           (type vector array))
  (cond ((or (zerop shift)
             (zerop (rem shift (- end start))))
         array)
        ((> shift 0) (circular-shift-right
                      array start end shift))
        (t (circular-shift-left
            array start end (- shift)))))


(defun in-shuffle-array (array start end &aux (length (- end start)))
  (declare (type vector array)
           (type non-negative-fixnum start end length)
           (optimize (speed 3)))
  (when (<= length 2)
    (return-from in-shuffle-array array))
  (let ((odd (oddp length)))
    (when odd (decf length) (incf start))
    (iterate
      (declare (type fixnum i m 2m n section-length
                     highest-leader))
      (with i = (if odd 0 1))
      (with m = 0)
      (with 2m = 0)
      (with n = 1)
      (while (< m n))
      (for section-length = (- length i))
      (for highest-leader = (highest-leader section-length))
      (setf m (if (> highest-leader 1)
                  (truncate highest-leader 2)
                  1))
      (setf 2m (the fixnum (* 2 m)))
      (setf n (truncate section-length 2))
      (circular-shift-right array
                            (the fixnum (+ i m start))
                            (the fixnum (+ i m n start))
                            m)
      (iterate
        (declare (type non-negative-fixnum leader))
        (with leader = 1)
        (while (< leader 2m))
        (cycle-rotate array
                      leader
                      (+ i start)
                      2m)
        (setf leader (the fixnum (* leader 3))))
      (incf i 2m)))
  array)


(defun inverse-in-shuffle-array (array start end &aux (length (- end start)))
  (declare (type vector array)
           (type non-negative-fixnum start end length)
           (optimize (debug 3)))
  (when (<= length 2)
    (return-from inverse-in-shuffle-array array))
  (let ((odd (oddp length)))
    (cl-ds.utils:lolol (length)
      (when odd (decf length))
      (iterate
        (with m = 0)
        (with 2m = 0)
        (with n = 0)
        (with i = 0)
        (for section-length = (- length i))
        (until (zerop section-length))
        (for highest-leader = (highest-leader section-length))
        (setf m (if (> highest-leader 1)
                    (truncate highest-leader 2)
                    1))
        (setf 2m (the fixnum (* 2 m)))
        (setf n (truncate section-length 2))
        (iterate
          (with shift = i)
          (with leader = 1)
          (while (< leader 2m))
          (for j = leader)
          (iterate
            (setf j (if (oddp j)
                        (+ m (truncate j 2))
                        (truncate j 2)))
            (rotatef (aref array (+ j start shift))
                     (aref array (+ leader start shift)))
            (until (eql j leader)))
          (setf leader (* leader 3)))
        (circular-shift-left array
                             (+ start (truncate i 2))
                             (+ start i m)
                             (truncate i 2))
        (incf i 2m)))
    (when odd
      (circular-shift-right array
                            (+ start (truncate length 2))
                            (+ start length)
                            1)))
  array)


(defun unfold-table (table)
  (make-array (reduce #'* (array-dimensions table))
              :element-type (array-element-type table)
              :displaced-to table))


(defun swap-blocks (vector from to size)
  (declare (type vector vector)
           (optimize (debug 3))
           (type non-negative-fixnum from to size))
  (let ((length (length vector)))
    (when (or (> (+ to size) length)
              (> (+ from size) length))
      (error "Block does not fit in the array!")))
  (iterate
    (for i from from)
    (for j from to)
    (repeat size)
    (rotatef (aref vector i) (aref vector j))
    (finally (return vector))))


(defun merge-in-place (array start end)
  (declare (optimize (debug 3)))
  (bind ((length (- end start))
         (curi 0)
         (nexti 1)
         (count 0)
         (block-size 0)
         (block-start 0)
         ((:flet at (index))
          (aref array (+ start index))))
    (in-shuffle-array array start end)
    (iterate
      (while (< curi length))
      (for prev-nexti = nexti)
      (if (> block-size 0)
          (iterate
            (while (and (< curi nexti) (< (at curi) (at nexti))))
            (incf curi))
          (setf block-start curi))
      (iterate
        (while (and (< nexti length)
                    (> (at curi) (at nexti))))
        (incf count)
        (setf nexti (+ nexti 2)))
      (if (> count 0)
          (progn
            (if (zerop block-size)
                (inverse-in-shuffle-array array
                                          (+ start curi)
                                          (+ start nexti -1))
                (progn
                  (unless (eql prev-nexti curi)
                    (inverse-in-shuffle-array  array
                                              (+ start prev-nexti -1)
                                              (+ nexti start)))
                  (swap-blocks array
                               (+ start block-start)
                               (+ start block-start (truncate length 2))
                               block-size)))
            (incf curi count))
          (cond ((and (> block-size 0)
                      (not (eql prev-nexti curi)))
                 (setf nexti (+ 2 curi)
                       block-size 0))
                ((> block-size 0)
                 (setf nexti (1+ curi))
                 (decf curi))
                (t (incf nexti))))
      (incf curi)
      (setf block-size (- nexti curi 1))
      ;; (assert (>= block-size 0))
      )
    array))


;; (defparameter *result* (merge-in-place (concatenate 'vector
;;                                                     (~> (make-array 32)
;;                                                         (map-into (curry #'random 1000))
;;                                                         (sort #'<))
;;                                                     (~> (make-array 32)
;;                                                         (map-into (curry #'random 5000))
;;                                                         (sort #'<)))
;;                                        0 64))
