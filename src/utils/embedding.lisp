(in-package #:cl-ds.utils)


(defun bourgain-embedding (vector-of-elements distance-fn
                           &key (embedding-type 'single-float))
  (declare (type vector vector-of-elements))
  (ensure-functionf distance-fn)
  (bind ((length (length vector-of-elements))
         (embeddings (make-array length))
         (k (ceiling (/ (log length) (1- (log 2)))))
         (sample (make-array (ash 1 k) :fill-pointer 0))
         (top (ceiling (log length))))
    (iterate
      (for i from 0 to k)
      (iterate
        (for h below top)
        (draw-random-vector vector-of-elements (ash 1 i) sample)
        (iterate
          (for x in-vector vector-of-elements)
          (for j from 0)
          (for distance = (iterate
                            (for other-x in-vector sample)
                            (minimize (funcall distance-fn x other-x))))
          (if (= 0 i h)
              (setf (aref embeddings j) (make-array top
                                                    :element-type embedding-type
                                                    :initial-element distance
                                                    :fill-pointer 1))
              (vector-push-extend distance (aref embeddings j))))))
    embeddings))
