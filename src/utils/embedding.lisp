(cl:in-package #:cl-ds.utils)


(defun bourgain-embedding (vector-of-elements distance-fn &key (metric-type 'single-float))
  (declare (type vector vector-of-elements))
  (ensure-functionf distance-fn)
  (bind ((length (length vector-of-elements))
         (m1 (floor (/ (log length) (log 2))))
         (m2 (ceiling (log length)))
         (embeddings (make-array (list length (* m1 m2))
                                 :element-type metric-type))
         (embeddings-view (make-array (list length m1 m2)
                                      :element-type metric-type
                                      :displaced-to embeddings))
         ((:flet random-sample (count))
          (iterate
            (with result = (make-array count))
            (for i from 0 below count)
            (setf (aref result i)
                  (aref vector-of-elements (random length)))
            (finally (return result)))))
    (declare (type fixnum length m1 m2)
             (type (simple-array * (* *)) embeddings))
    (iterate
      (for i from 0 below m1)
      (iterate
        (for j from 0 below m2)
        (for s = (random-sample (expt 2 i)))
        (iterate
          (for k from 0 below length)
          (for x = (aref vector-of-elements k))
          (for distance = (reduce #'min s
                                  :key (curry distance-fn x)))
          (setf (aref embeddings-view k i j) distance))))
    embeddings))
