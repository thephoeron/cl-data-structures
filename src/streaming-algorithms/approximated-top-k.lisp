(cl:in-package #:cl-ds.sa)


(cl-ds.alg.meta:define-aggregation-function
    approximated-top-k approximated-top-k-function

    (:range k &key hash-fn width depth key hashes data-sketch test)
    (:range k &key (hash-fn #'sxhash) width depth key hashes
     (test 'equal)
     (data-sketch (clean-sketch #'approximated-counts
                                :hashes hashes
                                :hash-fn hash-fn
                                :width width
                                :depth depth)))

    (%data-sketch %heap %k %test)

    ((check-type data-sketch approximated-counts)
     (check-type k integer)
     (cl-ds:check-argument-bounds k (> k 0))
     (setf %data-sketch (cl-ds:clone data-sketch)
           %k k
           %test (ensure-function test)
           %heap (make-array k :fill-pointer 0)))

    ((element)
     (let ((count (update-count-min-sketch element %data-sketch))
           (fill-pointer (fill-pointer %heap))
           (position (position element %heap :test %test
                                             :key #'car)))
       (cond ((not (null position))
              ;; count is overestimated, so simply incf value in the %heap is likely to be wrong.
              (setf (cdr (aref %heap position)) count)
              (sort %heap #'> :key #'cdr))
             ((< fill-pointer %k)
              (vector-push-extend (cons element count)
                                  %heap)
              (setf %heap (sort %heap #'> :key #'cdr)))
             (t
              (let ((split-point -1)
                    (needs-sort nil))
                (iterate
                  (for i from (1- fill-pointer) downto 0)
                  (for top.old-estimate = (aref %heap i))
                  (for (top . old-estimate) = top.old-estimate)
                  (when (> old-estimate count) (leave))
                  (setf needs-sort t)
                  (for new-estimate = (cl-ds:at %data-sketch top))
                  (setf (cdr top.old-estimate) new-estimate)
                  (when (> count new-estimate)
                    (setf split-point i)
                    (leave)))
                (iterate
                  (for i from 0 below split-point)
                  (for top.old-estimate = (aref %heap i))
                  (for (top . old-estimate) = top.old-estimate)
                  (setf (cdr top.old-estimate) (cl-ds:at %data-sketch top)))
                (cond ((< split-point 0)
                       (setf %heap (sort %heap #'> :key #'cdr)
                             (aref %heap (1- fill-pointer)) (cons element count)
                             %heap (sort %heap #'> :key #'cdr)))
                      (needs-sort
                       (setf %heap (sort %heap #'> :key #'cdr)))))))))

    ((iterate
       (for i from 0 below (length %heap))
       (for top.old-estimate = (aref %heap i))
       (for (top . old-estimate) = top.old-estimate)
       (setf (cdr top.old-estimate) (cl-ds:at %data-sketch top)))
     (setf %heap (sort %heap #'> :key #'cdr))
     (cl-ds:whole-range %heap)))
