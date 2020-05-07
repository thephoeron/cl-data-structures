(cl:in-package #:cl-data-structures.algorithms)


(defclass split-into-chunks-proxy (proxy-range)
  ((%count-in-chunk :initarg :count-in-chunk
                    :accessor access-count-in-chunk)))


(defmethod initialize-instance :after ((object split-into-chunks-proxy) &key)
  (check-type (access-count-in-chunk object) integer)
  (unless (< 0 (access-count-in-chunk object))
    (error 'cl-ds:argument-value-out-of-bounds
           :argument 'chunk-size
           :bounds '(< 0)
           :value (access-count-in-chunk object))))


(defclass forward-split-into-chunks-proxy
    (split-into-chunks-proxy fundamental-forward-range)
  ())


(defclass bidirectional-split-into-chunks-proxy
    (forward-split-into-chunks-proxy fundamental-bidirectional-range)
  ())


(defclass random-access-split-into-chunks-proxy
    (bidirectional-split-into-chunks-proxy fundamental-random-access-range)
  ())


(defmethod clone ((range split-into-chunks-proxy))
  (make-instance (type-of range)
                 :original-range (~> range read-original-range clone)
                 :count-in-chunk (access-count-in-chunk range)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range split-into-chunks-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (let ((outer-fn (call-next-method))
        (maximal-count-in-chunk (access-count-in-chunk range)))
    (declare (type fixnum maximal-count-in-chunk))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator ((chunks (vect))
                                     (count-in-chunk 0))

         ((element)
           (when (or (>= (the fixnum count-in-chunk) maximal-count-in-chunk)
                     (emptyp chunks))
             (vector-push-extend (cl-ds.alg.meta:call-constructor outer-fn) chunks)
             (setf count-in-chunk 0))
           (cl-ds.alg.meta:pass-to-aggregation (aref chunks (1- (fill-pointer chunks)))
                                               element)
           (incf count-in-chunk))

         ((~> #'cl-ds.alg.meta:extract-result
              (cl-ds.utils:transform chunks)
              cl-ds:whole-range))

       (iterate
         (for inner in-vector chunks)
         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))


(defclass split-into-chunks-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric split-into-chunks (range chunk-size)
  (:generic-function-class split-into-chunks-function)
  (:method (range chunk-size)
    (apply-range-function range #'split-into-chunks
                          (list range chunk-size))))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn split-into-chunks-function)
                        all)
  (make-proxy range 'forward-split-into-chunks-proxy
              :count-in-chunk (second all)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn split-into-chunks-function)
                        all)
  (make-proxy range 'bidirectional-split-into-chunks-proxy
              :count-in-chunk (second all)))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn split-into-chunks-function)
                        all)
  (make-proxy range 'random-access-split-into-chunks-proxy
              :count-in-chunk (second all)))
