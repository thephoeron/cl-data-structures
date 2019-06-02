(cl:in-package #:cl-ds.utils)


(defun bucket-sort (vector predicate
                    &key
                      (start 0)
                      (end nil end-bound-p)
                      (key #'identity)
                      (parallel t)
                      (buckets-count nil buckets-count-bound-p))
  (ensure-functionf key predicate)
  (check-type vector vector)
  (check-type parallel boolean)
  (unless end-bound-p
    (setf end (length vector)))
  (check-type end non-negative-fixnum)
  (check-type start non-negative-fixnum)
  (assert (<= start end (length vector)))
  (unless buckets-count-bound-p
    (setf buckets-count (~> (- end start)
                            (truncate 16))))
  (check-type buckets-count positive-fixnum)
  (bind (((:values min-elt max-elt) (extrema vector #'<
                                             :key key
                                             :start start
                                             :end end))
         (min (funcall key min-elt))
         (max (funcall key max-elt))
         (bucket-span (~> (- max min) (ceiling buckets-count)))
         (element-type (array-element-type vector))
         (buckets (map-into (make-array buckets-count
                                        :adjustable t
                                        :fill-pointer buckets-count)
                            (lambda () (make-array 16
                                                   :element-type element-type
                                                   :adjustable t
                                                   :fill-pointer 0)))))
    ;; assign each element to a bucket
    (iterate
      (declare (type fixnum i))
      (for i from start below end)
      (for elt = (aref vector i))
      (for value = (funcall key elt))
      (for bucket-index = (~> (- value min)
                              (round bucket-span)))
      (vector-push-extend value (aref buckets bucket-index)))
    ;; some buckets could have no elements assigned. delete those
    (decf (fill-pointer buckets)
          (cl-ds.utils:swap-if buckets #'emptyp))
    ;; sort individual buckets
    (if parallel
        (lparallel:pmap-into buckets
                             (lambda (bucket)
                               (sort bucket predicate :key key))
                             buckets)
        (map-into buckets
                  (lambda (bucket)
                    (sort bucket predicate :key key))
                  buckets))
    ;; order buckets properly
    (setf buckets
          (if parallel
              (lparallel:psort buckets predicate
                               :key (compose key #'first-elt))
              (sort buckets predicate
                    :key (compose key #'first-elt))))
    ;; assign buckets content back to the input vector
    (iterate
      (with i = start)
      (for bucket in-vector buckets)
      (iterate
        (for elt in-vector bucket)
        (setf (aref vector i) elt)
        (incf i)))
    vector))
