(in-package #:cl-data-structures.utils)


(-> insert-or-replace (list t &key
                            (:test (-> (t t) boolean))
                            (:list-key (-> (t) t))
                            (:item-key (-> (t) t))
                            (:preserve-order boolean))
    (values list boolean t))
(declaim (inline insert-or-replace))
(defun insert-or-replace (list element &key
                                         (test #'eql)
                                         (list-key #'identity)
                                         (item-key #'identity)
                                         (preserve-order nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "Insert element into set if it is not already here.

   @b(Returns three values:)
   @begin(list)
    @item(first -- new list)
    @item(second -- was any item replaced?)
    @item(third -- old value that was replaced (or nil if there was no such value))
   @end(list)"
  (iterate
    (with last-cell = nil)
    (with result = nil)
    (with replaced = nil)
    (with value = nil)
    (for sublist on list)
    (for elt = (car sublist))
    (if (funcall test (funcall list-key elt) (funcall item-key element))
        (progn
          (push element result)
          (setf replaced t
                value elt))
        (push elt result))
    (unless last-cell
      (setf last-cell result))
    (when (and replaced last-cell (not preserve-order))
      (setf (cdr last-cell)
            (cdr sublist))
      (finish))
    (finally (return (values (let ((r (if preserve-order
                                          (nreverse result)
                                          result)))
                               (if replaced
                                   r
                                   (cons element
                                         r)))
                             replaced
                             value)))))


(-> try-remove (t list &key
                  (:test (-> (t t) boolean))
                  (:key (-> (t) t))
                  (:preserve-order boolean))
    (values list boolean t))
(declaim (inline try-remove))
(defun try-remove (item list &key (test #'eql) (key #'identity) (preserve-order nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "Try to remove first item matching from the list.

   @b(Returns three values:)
   @begin(list)
    @item(first -- new list)
    @item(second -- did anything was removed?)
    @item(third -- value that was removed (or nil if nothing was removed))
   @end(list)"
  (iterate
    (for sublist on list)
    (for elt = (car sublist))
    (with removed = nil)
    (with value = nil)
    (with last-cell = nil)
    (if (funcall test
                 (funcall key elt)
                 item)
        (setf removed t
              value elt)
        (collect elt into result at start))
    (unless last-cell
      (setf last-cell result))
    (when (and removed last-cell (not preserve-order))
      (setf (cdr last-cell) (cdr sublist))
      (finish))
    (finally (return (values (if preserve-order
                                 (reverse result)
                                 result)
                             removed
                             value)))))


(-> try-find-cell (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t))) list)
(defun try-find-cell (item list &key (test #'eql) (key #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "@b(Returns) first matching sublist"
  (iterate
    (for elt on list)
    (when (funcall test
                   (funcall key (car elt))
                   item)
      (leave elt))))


(-> try-find-cell (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t))) (values t boolean))
(defun try-find (item list &key (test #'eql) (key #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "@b(Returns) first matching elements as first value and boolean telling if it was found as second"
  (let ((r (try-find-cell item list :test test :key key)))
    (values (car r)
            (not (null r)))))


(defun lexicographic-compare (compare same av bv &key (key #'identity))
  (setf compare (alexandria:ensure-function compare))
  (setf same (alexandria:ensure-function same))
  (check-type av sequence)
  (check-type bv sequence)
  (check-type compare function)
  (check-type same function)
  (iterate
    (for ea1 in-sequence av)
    (for eb1 in-sequence bv)
    (for ea = (funcall key ea1))
    (for eb = (funcall key eb1))
    (for sm = (funcall same ea eb))
    (for comp = (funcall compare ea eb))
    (finding comp such-that comp into r)
    (always sm)
    (finally
     (if r
         (return r)
         (when sm
           (return (< (length av) (length bv))))))))


(defun add-to-list (list data &optional (function #'identity))
  (reduce (flip #'cons)
          data
          :initial-value list
          :key function))


(defun normalize-sequence-to-sum (sequence sum)
  (declare (type sequence sequence)
           (type number sum))
  (let* ((current-sum (reduce #'+ sequence))
         (ratio (/ sum current-sum)))
    (map-into sequence (curry #'* ratio) sequence)))


(defun normalize-sequence-to-span (sequence min max)
  (declare (type sequence sequence)
           (type number min max))
  (assert (< min max))
  (let* ((current-min (reduce #'min sequence))
         (current-max (reduce #'max sequence))
         (current-span (- current-max current-min))
         (diff (- 0 current-min))
         (ratio (/ (- max min) current-span)))
    (map-into sequence
              (compose (curry #'+ min)
                       (curry #'* ratio)
                       (curry #'+ diff))
              sequence)))


(defun remove-fill-pointer (vector)
  (check-type vector vector)
  (if (array-has-fill-pointer-p vector)
      (map `(vector ,(array-element-type vector)) #'identity vector)
      vector))


(defun select-top (vector count predicate &key (key #'identity))
  (declare (type non-negative-fixnum count)
           (optimize (speed 3) (safety 1)
                     (space 0) (debug 0)))
  (check-type vector vector)
  (ensure-functionf predicate key)
  (nest
   (cases ((eq key #'identity)
           (simple-vector-p vector)
           (:variant
            (typep vector '(vector fixnum))
            (typep vector '(vector non-negative-fixnum))
            (typep vector '(vector single-float))
            (typep vector '(vector double-float)))
           (:variant
            (eq predicate #'>)
            (eq predicate #'<))))
   (let* ((length (length vector))
          (count (min length count))
          (result (make-array count
                              :element-type (array-element-type vector)))
          (heap-size length))
     (declare (type fixnum heap-size count length)))
   (labels ((compare (a b)
              (declare (type fixnum a b))
              (funcall predicate
                       (funcall key (aref vector a))
                       (funcall key (aref vector b))))
            (left (i)
              (declare (type fixnum i))
              (the fixnum (1+ (the fixnum (* 2 i)))))
            (right (i)
              (declare (type fixnum i))
              (the fixnum (+ 2 (the fixnum (* 2 i)))))
            (heapify (i)
              (declare (type fixnum i))
              (iterate
                (declare (type fixnum l r smallest))
                (for l = (left i))
                (for r = (right i))
                (for smallest = i)
                (when (and (< l heap-size)
                           (compare l i))
                  (setf smallest l))
                (when (and (< r heap-size)
                           (compare r smallest))
                  (setf smallest r))
                (if (eql smallest i)
                    (leave)
                    (psetf i smallest
                           (aref vector smallest) (aref vector i)
                           (aref vector i) (aref vector smallest)))))
            (extract-min ()
              (when (> heap-size 1)
                (setf (aref vector 0)
                      (aref vector (1- heap-size)))
                (heapify 0))
              (decf heap-size)))
     (declare (inline extract-min heapify
                      right left compare))
     (iterate
       (declare (type fixnum i start))
       (with start = (truncate (1- heap-size) 2))
       (for i from start downto 0)
       (heapify i))
     (iterate
       (declare (type fixnum i))
       (for i from 0 below count)
       (setf (aref result i) (aref vector 0))
       (extract-min))
     result)))
