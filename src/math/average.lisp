(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    average average-function
    (:range &key key sum count)
    (:range &key (key #'identity) (sum 0) (count 0))
    ((%sum number) (%count integer))
    ((setf %sum sum
           %count count))
    ((element)
     (incf %count)
     (incf %sum element))
    ((/ %sum %count)))


(cl-ds.alg.meta:define-aggregation-function
    harmonic-average harmonic-average-function
    (:range &key key count sum)
    (:range &key (key #'identity) (count 0) (sum 0))
    ((%sum number) (%count integer) (%zero boolean))
    ((setf %sum sum
           %count count
           %zero nil))
    ((element)
     (incf %count)
     (if (zerop element)
         (setf %zero t)
         (incf %sum (/ 1 element))))
    ((if %zero
         0.0
         (/ %count %sum))))


(cl-ds.alg.meta:define-aggregation-function
    array-average array-average-function
    (:range &key key sum count)
    (:range &key (key #'identity) (sum nil) (count 0))
    ((%sum (or array null)) (%count integer))
    ((setf %count count
           %sum (if (null sum)
                    nil
                    (copy-array sum))))
    ((element)
     (incf %count)
     (when (null %sum)
       (setf %sum (make-array (array-dimensions element)
                              :initial-element 0.0)))
     (iterate
       (for i from 0 below (array-total-size %sum))
       (incf (row-major-aref %sum i) (row-major-aref element i))))
    ((iterate
       (for i from 0 below (array-total-size %sum))
       (setf (row-major-aref %sum i) (/ (row-major-aref %sum i)
                                        %count))
       (finally (return %sum)))))


(cl-ds.alg.meta:define-aggregation-function
    array-harmonic-average array-harmonic-average-function
    (:range &key key sum count)
    (:range &key (key #'identity) (sum nil) (count 0))
    ((%sum (or array null))
     (%count integer)
     (%zeros (or simple-array null)))
    ((setf %count count
           %zeros (if (null sum)
                      nil
                      (make-array (array-dimensions sum)
                                  :element-type 'boolean
                                  :initial-element nil))
           %sum (if (null sum)
                    nil
                    (copy-array sum))))
    ((element)
     (incf %count)
     (when (null %zeros)
       (setf %zeros (make-array (array-dimensions element)
                                :element-type 'boolean
                                :initial-element nil)))
     (when (null %sum)
       (setf %sum (setf %sum (make-array (array-dimensions element)
                                         :initial-element 0.0))))
     (iterate
       (for i from 0 below (array-total-size %sum))
       (if (zerop (row-major-aref element i))
           (setf (row-major-aref %zeros i) t)
           (incf (row-major-aref %sum i)
                 (/ 1 (row-major-aref element i))))))
    ((iterate
       (for i from 0 below (array-total-size %sum))
       (if (row-major-aref %zeros i)
           (setf (row-major-aref %sum i) 0)
           (setf (row-major-aref %sum i) (/ %count
                                            (row-major-aref %sum i))))
       (finally (return %sum)))))
