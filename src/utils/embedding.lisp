(in-package #:cl-ds.utils)


(defun bourgain-embedding (vector-of-elements distance-fn
                           &key (embedding-type 'single-float) (parallel t))
  (declare (type vector vector-of-elements)
           (type boolean parallel))
  (ensure-functionf distance-fn)
  (bind ((length (length vector-of-elements))
         (embeddings (make-array length))
         (k (ceiling (/ (log length) (1- (log 2)))))
         (sample (make-array (ash 1 k) :fill-pointer 0))
         (top (ceiling (log length))))
    (map-into embeddings (curry #'make-array top
                                :element-type embedding-type
                                :fill-pointer 0))
    (iterate
      (for i from 0 to k)
      (iterate
        (for h below top)
        (draw-random-vector vector-of-elements (ash 1 i) sample)
        (funcall (if parallel #'lparallel:pmap #'map) nil
                 (lambda (embedding x)
                   (iterate
                     (for other-x in-vector sample)
                     (minimize (funcall distance-fn x other-x) into distance)
                     (finally (vector-push-extend distance embedding))))
                 embeddings
                 vector-of-elements)))
    embeddings))
