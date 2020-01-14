(cl:in-package #:cl-data-structures.math)


(defun fast-map-embeddings (data metric-function dimensions iterations)
  (bind ((length (length data))
         (distance-matrix (cl-ds.utils:make-distance-matrix-from-vector
                           t metric-function data))
         (result (make-array `(,length ,dimensions)
                             :element-type 'single-float
                             :initial-element 0.0f0))
         ((:labels distance (a b axis))
          (declare (optimize (debug 3) (speed 0)))
          (if (zerop axis)
              (if (= a b)
                  0.0
                  (expt (cl-ds.utils:mref distance-matrix a b)
                        2))
              (let* ((rec (distance a b (1- axis)))
                     (resd (~> (aref result a axis)
                               (- (aref result b axis))
                               (expt 2)))
                     (result (abs (- rec resd))))
                result)))
         ((:flet furthest (o col))
          (iterate
            (for i from 0 below length)
            (when (= i o)
              (next-iteration))
            (finding i maximizing (distance i o col))))
         ((:flet select-pivots (col))
          (iterate
            (with o1 = (random length))
            (with o2 = -1)
            (with o = -1)
            (repeat iterations)
            (setf o (furthest o1 col))
            (when (= o o2) (finish))
            (setf o2 o)
            (setf o (furthest o2 col))
            (when (= o o1) (finish))
            (setf o1 o)
            (finally (return (cons o1 o2)))))
         ((:flet x (i x y col))
          (bind ((dix (distance i x col))
                 (diy (distance i y col))
                 (dxy (distance x y col)))
            (/ (+ dix dxy diy)
               (* 2 (sqrt dxy)))))
         ((:labels impl (col))
          (when (= col dimensions)
            (return-from impl nil))
          (bind (((first-distant . second-distant) (select-pivots col))
                 (distance (distance first-distant second-distant col)))
            (when (< distance single-float-epsilon)
              (return-from impl nil))
            (iterate
              (for i from 0 below length)
              (setf (aref result i col) (x i first-distant second-distant col)))
            (impl (1+ col)))))
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
