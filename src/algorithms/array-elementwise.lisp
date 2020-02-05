(cl:in-package #:cl-data-structures.algorithms)


(defclass array-elementwise-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric array-elementwise (range)
  (:generic-function-class array-elementwise-function)
  (:method (range)
    (apply-range-function range #'array-elementwise (list range))))


(defclass in-array-forward-proxy (forward-proxy-range)
  ())


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (function array-elementwise-function)
                                       arguments)
  (make 'in-array-forward-proxy :original-range range))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range in-array-forward-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (let ((outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((inners nil))

         ((element)
           (check-type element array)
           (when (null inners)
             (setf inners (make-array (array-dimensions element)))
             (iterate
               (for i from 0 below (array-total-size inners))
               (setf (row-major-aref inners i)
                     (cl-ds.alg.meta:call-constructor outer-fn))))
           (iterate
             (for i from 0 below (array-total-size inners))
             (cl-ds.alg.meta:pass-to-aggregation (row-major-aref inners i)
                                                 (row-major-aref element i))))

         ((unless (null inners)
            (iterate
              (for i from 0 below (array-total-size inners))
              (setf #1=(row-major-aref inners i)
                    (cl-ds.alg.meta:extract-result #1#)))))

       (unless (null inners)
         (iterate
           (for i from 0 below (array-total-size inners))
           (cl-ds.alg.meta:cleanup (row-major-aref inners i)))))
     function
     arguments)))
