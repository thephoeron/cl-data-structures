(in-package #:cl-data-structures.streaming-algorithms)


(define-constant +long-prime+ 4294967311)


(cl-ds.alg.meta:define-aggregation-function
    approximated-counts approximated-counts-function

  (:range hash-fn epsilon gamma &key key)
  (:range hash-fn epsilon gamma &key (key #'identity))

  (%width %depth %hash-fn %epsilon %gamma %aj %bj %total %counters %hashes)

  ((&key hash-fn epsilon gamma)
   (check-type epsilon real)
   (check-type gamma real)
   (ensure-functionf hash-fn)
   (unless (and (<= 0.009 epsilon)
                (< epsilon 1))
     (error 'cl-ds:argument-out-of-bounds
            :bounds '(0.009 1)
            :value epsilon
            :argument 'epsilon
            :text "Epsilon out of bounds."))
   (unless (< 0 gamma 1)
     (error 'cl-ds:argument-out-of-bounds
            :bounds '(0 1)
            :value gamma
            :argument 'gamma
            :text "Gamma out of bounds."))
   (setf %hash-fn hash-fn
         %width (ceiling (/ (exp 1) epsilon))
         %depth (ceiling (log (/ 1 gamma)))
         %gamma gamma
         %total 0
         %epsilon epsilon
         %counters (make-array (list %depth %width) :initial-element 0)
         %hashes (make-array (list %depth 2) :element-type 'fixnum))
   (map-into (cl-ds.utils:unfold-table %hashes)
             (lambda () (truncate (1+ (/ (* (random most-positive-fixnum)
                                       +long-prime+)
                                    (1- most-positive-fixnum)))))))

  ((element)
   (incf %total)
   (iterate
     (with hash = (funcall %hash-fn element))
     (for j from 0 below %depth)
     (for hashval = (~> (aref %hashes j 0)
                        (* hash)
                        (+ (aref %hashes j 1))
                        (rem +long-prime+)
                        (rem %width)))
     (incf (aref %counters j hashval))))

  (cl-ds.utils:todo))
