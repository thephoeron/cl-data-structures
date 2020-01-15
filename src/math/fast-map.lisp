(cl:in-package #:cl-data-structures.math)


(defun fast-map-embeddings (data metric-function dimensions iterations)
  (bind ((length (length data))
         (distance-matrix (cl-ds.utils:make-distance-matrix-from-vector
                           t metric-function data))
         (result (make-array `(,length ,dimensions)
                             :element-type 'single-float
                             :initial-element 0.0f0))
         ((:labels distance (a b axis))
          (if (zerop axis)
              (if (= a b)
                  0.0f0
                  (cl-ds.utils:mref distance-matrix a b))
              (~> (distance a b (1- axis))
                  (expt 2)
                  (- (expt (- (aref result a (1- axis))
                              (aref result b (1- axis)))
                           2))
                  sqrt)))
         ((:flet furthest (o axis))
          (iterate
            (for i from 0 below length)
            (for distance = (distance i o axis))
            (finding i maximizing distance into result)
            (finally (print i) (print o) (return result))))
         ((:flet select-pivots (axis))
          (iterate
            (with o1 = (random length))
            (with o2 = -1)
            (with o = -1)
            (repeat iterations)
            (setf o (furthest o1 axis))
            (when (= o o2) (finish))
            (shiftf o2 o (furthest o axis))
            (when (= o o1) (finish))
            (setf o1 o)
            (finally
             (return (cons o1 o2)))))
         ((:flet project (i x y axis dxy))
          (bind ((dix (expt (distance i x axis) 2))
                 (diy (expt (distance i y axis) 2)))
            (/ (+ dix dxy (- diy))
               (* 2 dxy))))
         ((:labels impl (axis))
          (when (= axis dimensions)
            (return-from impl nil))
          (bind (((first-distant . second-distant) (select-pivots axis))
                 (distance (distance first-distant second-distant axis)))
            (iterate
              (for i from 0 below length)
              (setf (aref result i axis)
                    (cond ((= i first-distant) 0.0f0)
                          ((= i second-distant) distance)
                          (t (project i first-distant
                                      second-distant axis distance))))
              (finally (impl (1+ axis)))))))
    (impl 0)
    result))


(cl-ds.alg.meta:define-aggregation-function
    fast-map fast-map-function

    (:range metric-function dimensions iterations &key key)
    (:range metric-function dimensions iterations &key (key #'identity))

    (%data %distance-function %dimensions %iterations)

    ((&key metric-function dimensions iterations &allow-other-keys)
     (setf %data (vect)
           %distance-function metric-function
           %iterations iterations
           %dimensions dimensions))

    ((element)
     (vector-push-extend element %data))

    ((fast-map-embeddings %data %distance-function %dimensions %iterations)))
