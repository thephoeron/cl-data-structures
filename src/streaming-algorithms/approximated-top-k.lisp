(cl:in-package #:cl-ds.sa)


(defun move-updated (vector position)
  (iterate
    (for i from position downto 0)
    (for j from (1- position) downto 0)
    (for (front-elt . front-count) = (aref vector j))
    (for (back-elt . back-count) = (aref vector i))
    (if (> back-count front-count)
        (rotatef (aref vector i) (aref vector j))
        (finish))
    (finally (return vector))))


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

    (%data-sketch %prototype-data-sketch %heap %k %test)

    ((check-type data-sketch approximated-counts)
     (check-type k integer)
     (cl-ds:check-argument-bounds k (> k 0))
     (setf %prototype-data-sketch data-sketch
           %data-sketch nil
           %k k
           %test (ensure-function test)
           %heap (make-array k :fill-pointer 0)))

    ((element)
     (bind ((fill-pointer (fill-pointer %heap))
            (position (position element %heap :test %test
                                              :key #'car)))
       (if (null %data-sketch)
           (cond ((not (null position))
                  (incf (cdr (aref %heap position)))
                  (move-updated %heap position))
                 ((< fill-pointer %k)
                  (vector-push-extend (cons element 1) %heap))
                 (t
                  (setf %data-sketch (cl-ds:clone %prototype-data-sketch))
                  (iterate
                    (for (top . count) in-vector %heap)
                    (update-count-min-sketch top %data-sketch count))
                  (update-count-min-sketch element %data-sketch)))
           (let ((count (update-count-min-sketch element %data-sketch)))
             (cond ((not (null position))
                    ;; count is overestimated, so simply incf value in the %heap is likely to be wrong.
                    (setf (cdr (aref %heap position)) count)
                    (move-updated %heap position))
                   ((< fill-pointer %k)
                    (vector-push-extend (cons element count)
                                        %heap)
                    (move-updated %heap position))
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
                                   (aref %heap (1- fill-pointer)) (cons element count))
                             (move-updated %heap (1- fill-pointer)))
                            (needs-sort
                             (setf %heap (sort %heap #'> :key #'cdr)))))))))))

    ((unless (null %data-sketch)
       (iterate
         (for i from 0 below (length %heap))
         (for top.old-estimate = (aref %heap i))
         (for (top . old-estimate) = top.old-estimate)
         (setf (cdr top.old-estimate) (cl-ds:at %data-sketch top))))
     (cl-ds:whole-range (sort %heap #'> :key #'cdr))))
