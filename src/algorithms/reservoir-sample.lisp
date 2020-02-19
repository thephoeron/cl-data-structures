(cl:in-package #:cl-data-structures.algorithms)


(defun gen-w (count)
  (exp (/ (log (random 1.0))
          count)))


(defun skip-count (count)
  (let ((w (gen-w count)))
    (1+ (floor (/ (log (random 1.0))
                  (log (- 1 w)))))))


(cl-ds.alg.meta:define-aggregation-function
    reservoir-sample reservoir-sample-function

    (:range sample-size &key key)
    (:range sample-size &key (key #'identity))

    ((%result (cl-ds.utils:extendable-vector t))
     (%sample-size positive-fixnum)
     (%skip-count (or null fixnum)))

    ((check-type sample-size positive-fixnum)
     (setf %result (make-array sample-size
                               :fill-pointer 0)
           %sample-size sample-size
           %skip-count nil))

    ((element)
     (if (and (null %skip-count)
              (< (length %result) %sample-size))
         (vector-push-extend element %result)
         (progn
           (when (null %skip-count)
             (setf %skip-count (skip-count %sample-size)))
           (decf %skip-count)
           (when (zerop %skip-count)
             (setf (aref %result (random (length %result))) element
                   %skip-count (skip-count %sample-size))))))

    (%result))


(defclass forward-with-reservoir-sample-proxy (proxy-range
                                               fundamental-forward-range)
  ((%sample-size :initarg :sample-size
                 :reader read-sample-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range forward-with-reservoir-sample-proxy))
  '((:sample-size read-sample-size)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range forward-with-reservoir-sample-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (let ((key (ensure-function (read-key range)))
        (sample-size (read-sample-size range))
        (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator ((sub (cl-ds.alg.meta:construct-aggregator
                                           nil
                                           #'reservoir-sample
                                           (list '() sample-size :key key))))
         ((element) (cl-ds.alg.meta:pass-to-aggregation sub element))
         ((iterate
            (with inner = (cl-ds.alg.meta:call-constructor outer-fn))
            (for v in-vector (cl-ds.alg.meta:extract-result sub))
            (cl-ds.alg.meta:pass-to-aggregation inner v)
            (finally (return (cl-ds.alg.meta:extract-result inner)))))))))


(defclass with-reservoir-sample-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric with-reservoir-sample (range sample-size &key key)
  (:generic-function-class with-reservoir-sample-function)
  (:method (range sample-size &key (key #'identity))
    (apply-range-function range #'with-reservoir-sample
                          (list range sample-size :key key))))


(defmethod apply-layer ((range cl-ds:traversable)
                        (fn with-reservoir-sample-function)
                        all)
  (let ((arguments (cddr all))
        (sample-size (second all)))
    (check-type sample-size positive-fixnum)
    (make-proxy range 'forward-with-reservoir-sample-proxy
                :key (getf arguments :key)
                :sample-size sample-size)))
