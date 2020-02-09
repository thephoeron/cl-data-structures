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
              (setf (cdr (aref %heap position)) count) ; count is overestimated, so simply incf value is likely to be wrong.
              (sort %heap #'> :key #'cdr))
             ((< fill-pointer %k)
              (vector-push-extend (cons element count)
                                  %heap)
              (setf %heap (sort %heap #'> :key #'cdr)))
             ((> count (~>> fill-pointer 1- (aref %heap) cdr))
              (setf (aref %heap (1- fill-pointer)) (cons element count)
                    %heap (sort %heap #'> :key #'cdr)))
             (t nil))))

    ((cl-ds:whole-range %heap)))
