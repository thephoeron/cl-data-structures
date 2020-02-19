(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    reservoir-sample reservoir-sample-function

    (:range sample-size &key key)
    (:range sample-size &key (key #'identity))

    ((%result simple-vector)
     (%pointer fixnum)
     (%skip-count (or null fixnum)))

    ((setf %result (make-array sample-size)
           %pointer 0
           %skip-count nil))

    ((element)
     (if (and (null %skip-count)
              (< %pointer (length %result)))
         (setf (aref %result %pointer) element
               %pointer (1+ %pointer))
         (labels ((gen-w ()
                  (exp (/ (log (random 1.0))
                          (length %result))))
                  (skip-count (&optional (w (gen-w)))
                    (+ (floor (/ (log (random 1.0))
                                 (log (- 1 w))))
                       1)))
           (when (null %skip-count)
             (setf %skip-count (skip-count)))
           (if (zerop %skip-count)
               (setf (aref %result (random (length %result))) element
                     %skip-count (skip-count))
               (decf %skip-count)))))

    (%result))
