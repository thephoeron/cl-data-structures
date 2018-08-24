(in-package #:cl-ds.utils)


(defun bourgain-embedding (vector-of-elements c distance-fn
                           &key (embedding-type 'single-float) (parallel t))
  (declare (type vector vector-of-elements)
           (type positive-fixnum c)
           (type boolean parallel))
  (ensure-functionf distance-fn)
  (bind ((length (length vector-of-elements))
         (embeddings (make-array length))
         (m1 (ceiling (log length 2)))
         (m2 (ceiling (* c (log length 2))))
         (element-type (array-element-type vector-of-elements))
         ((:dflet sample-size (j))
          (ceiling (* length (expt 2 (- j)))))
         (sample (make-array (sample-size 1)
                             :fill-pointer 0
                             :element-type element-type)))
    (map-into embeddings (curry #'make-array (* m1 m2)
                                :element-type embedding-type
                                :fill-pointer 0))
    (iterate
      (for j from 1 to m1)
      (iterate
        (for i from 1 to m2)
        (draw-sample-vector vector-of-elements (sample-size j) sample)
        (funcall (if parallel #'lparallel:pmap #'map) nil
                 (lambda (embedding x)
                   (iterate
                     (for other-x in-vector sample)
                     (minimize (funcall distance-fn x other-x) into distance)
                     (finally (vector-push-extend distance embedding))))
                 embeddings
                 vector-of-elements)))
    embeddings))

