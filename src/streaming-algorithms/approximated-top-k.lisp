(cl:in-package #:cl-ds.sa)


(cl-ds.alg.meta:define-aggregation-function
    approximated-top-k approximated-top-k-function

    (:range k &key hash-fn space count key hashes data-sketch)
    (:range k &key (hash-fn #'identity) space count key hashes
     (data-sketch (clean-sketch #'approximated-counts
                                :hashes hashes
                                :hash-fn hash-fn
                                :space space
                                :count count)))

    (%data-sketch %heap %k)

    ((check-type data-sketch approximated-counts)
     (check-type k integer)
     (cl-ds:check-argument-bounds k (> k 0))
     (setf %data-sketch data-sketch
           %k k
           %heap (make-array k :fill-pointer 0)))

    ((element)
     (let ((count (update-count-min-sketch element %data-sketch))
           (fill-pointer (fill-pointer %heap)))
       (cond ((< fill-pointer %k)
              (vector-push-extend (cons element count)
                                  %heap)
              (setf %heap (sort %heap #'> :key #'cdr)))
             ((> count (~>> fill-pointer 1- (aref %heap) cdr))
              (setf (aref %heap (1- fill-pointer)) (cons element count)
                    %heap (sort %heap #'> :key #'cdr)))
             (t nil))))

    ((cl-ds:whole-range %heap)))
