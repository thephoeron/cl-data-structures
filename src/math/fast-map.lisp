(cl:in-package #:cl-data-structures.math)


(defun fast-map-embeddings (data metric-function dimensions iterations parallel)
  (declare (type (cl-ds.utils:extendable-vector t) data)
           (type fixnum dimensions iterations)
           (optimize (speed 3)))
  (ensure-functionf metric-function)
  (assert (array-has-fill-pointer-p data))
  (bind ((length (length data))
         (result (make-array `(,length ,dimensions)
                             :element-type 'single-float
                             :initial-element 0.0f0))
         ((:labels distance (a b axis))
          (declare (type fixnum axis a b))
          (if (zerop axis)
              (if (= a b)
                  0.0f0
                  (coerce (funcall metric-function
                                   (aref data a)
                                   (aref data b))
                          'single-float))
              (let ((axis-1 (1- axis)))
                (declare (fixnum axis-1))
                (~> (distance a b axis-1)
                    (expt 2)
                    (- (expt (- (aref result a axis-1)
                                (aref result b axis-1))
                             2))
                    sqrt))))
         ((:flet furthest (o axis))
          (declare (type fixnum axis o))
          (iterate
            (declare (type fixnum i))
            (for i from 0 below length)
            (finding i maximizing (the single-float (distance i o axis)))))
         ((:flet select-pivots (axis))
          (declare (type fixnum axis))
          (iterate
            (declare (type fixnum o1 o2 o i))
            (with o1 = (random length))
            (with o2 = -1)
            (with o = -1)
            (for i from 0 below iterations)
            (setf o (furthest o1 axis))
            (when (= o o2) (finish))
            (shiftf o2 o (furthest o axis))
            (when (= o o1) (finish))
            (setf o1 o)
            (finally
             (return (cons o1 o2)))))
         ((:flet project (i x y axis dxy))
          (declare (type fixnum i x y axis)
                   (single-float dxy))
          (let ((dix (expt (the single-float (distance i x axis)) 2))
                (diy (expt (the single-float (distance i y axis)) 2)))
            (declare (type single-float dix diy))
            (/ (+ dix dxy (- diy))
               (* 2 dxy))))
         (indexes (coerce (iota length) '(vector fixnum)))
         ((:labels impl (axis))
          (declare (type fixnum axis))
          (when (= axis dimensions)
            (return-from impl nil))
          (bind (((first-distant . second-distant) (select-pivots axis))
                 (distance (the single-float (distance first-distant second-distant axis))))
            (when (zerop distance)
              (return-from impl nil))
            (funcall (if parallel #'lparallel:pmap #'map)
                     nil
                     (lambda (i)
                       (declare (type fixnum i))
                       (setf (aref result i axis)
                             (cond ((= i (the fixnum first-distant)) 0.0f0)
                                   ((= i (the fixnum second-distant)) distance)
                                   (t (project i first-distant
                                               second-distant axis distance)))))
                   indexes)
            (impl (1+ axis)))))
    (impl 0)
    result))


(cl-ds.alg.meta:define-aggregation-function
    fast-map fast-map-function

    (:range metric-function dimensions iterations &key key parallel)
    (:range metric-function dimensions iterations &key (key #'identity) parallel)

    (%data %distance-function %dimensions %iterations %parallel)

    ((setf %data (vect)
           %distance-function metric-function
           %parallel parallel
           %iterations iterations
           %dimensions dimensions))

    ((element)
     (vector-push-extend element %data))

    ((fast-map-embeddings %data %distance-function %dimensions %iterations %parallel)))
