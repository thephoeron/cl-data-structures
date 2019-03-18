(in-package #:cl-ds.utils)


(defun bourgain-embedding (vector-of-elements c distance-fn
                           &key
                             (embedding-type 'single-float) (parallel t)
                             (m2 (ceiling (log (length vector-of-elements)))))
  (declare (type vector vector-of-elements)
           (type fixnum m2)
           (optimize (speed 3))
           (type positive-fixnum c)
           (type boolean parallel))
  (ensure-functionf distance-fn)
  (check-type vector-of-elements vector)
  (nest
   (cases ((simple-vector-p vector-of-elements)
           (array-has-fill-pointer-p vector-of-elements)
           parallel))
   (let* ((length (length vector-of-elements))
          (embeddings (make-array length))
          (m1 (ceiling (1- (/ m2 (log 2)))))
          (element-type (array-element-type vector-of-elements)))
     (declare (type fixnum length m1 m2)
              (type simple-vector embeddings)))
   (bind ((sample (make-array (expt 2 m1)
                              :fill-pointer 0
                              :element-type element-type)))
     (map-into embeddings (curry #'make-array (the fixnum (* m1 m2))
                                 :element-type embedding-type
                                 :fill-pointer 0))
     (iterate
       (declare (type fixnum i))
       (for i from 1 to m1)
       (iterate
         (declare (type fixnum j))
         (for j from 1 to m2)
         (draw-sample-vector vector-of-elements (expt 2 i) sample)
         (funcall (if parallel #'lparallel:pmap #'map) nil
                  (lambda (embedding x)
                    (iterate
                      (for other-x in-vector sample)
                      (minimize (funcall distance-fn x other-x) into distance)
                      (finally (vector-push-extend distance embedding))))
                  embeddings
                  vector-of-elements)))
     embeddings)))
