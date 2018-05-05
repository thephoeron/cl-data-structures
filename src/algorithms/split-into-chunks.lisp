(in-package #:cl-data-structures.algorithms)


(defclass split-into-chunks-proxy (proxy-range)
  ((%count-in-chunk :initarg :count-in-chunk
                    :reader read-count-in-chunk)))


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
                 :count-in-chunk (read-count-in-chunk range)))


(defclass split-into-chunks (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass split-into-chunks-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%chunks :initform (vect)
            :initarg :chunks
            :type vector
            :reader read-chunks)
   (%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%maximal-count-in-chunk :initarg :maximal-count-in-chunk
                            :reader read-maximal-count-in-chunk)
   (%count-in-chunk :initarg :count-in-chunk
                    :initform 0
                    :accessor access-count-in-chunk)))


(defclass linear-split-into-chunks-aggregator (split-into-chunks-aggregator)
  ((%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defclass multi-split-into-chunks-aggregator (split-into-chunks-aggregator
                                              multi-aggregator)
  ((%stages :initarg :stages
            :accessor acess-stages)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator split-into-chunks-aggregator))
  (iterate
    (for chunk in-vector (read-chunks aggregator))
    (begin-aggregation chunk)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator split-into-chunks-aggregator))
  (iterate
    (for chunk in-vector (read-chunks aggregator))
    (end-aggregation chunk)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator multi-split-into-chunks-aggregator))
  (assert (access-stages aggregator))
  (call-next-method)
  (setf (access-stages aggregator) (rest (access-stages aggregator))))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator linear-split-into-chunks-aggregator))
  (setf (access-finished aggregator) t)
  (call-next-method))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator multi-split-into-chunks-aggregator))
  (~> aggregator access-stages endp))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator linear-split-into-chunks-aggregator))
  t)


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator multi-split-into-chunks-aggregator))
  (cl-ds.alg.meta:expects-content-with-stage-p (~> aggregator access-stages first)
                                               aggregator))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator split-into-chunks-aggregator)
                                               element)
  (bind (((:slots %chunks %maximal-count-in-chunk
                  %outer-fn %count-in-chunk)
          aggregator)
         (length (length %chunks))
         (last (1- length)))
    (when (zerop (rem %count-in-chunk %maximal-count-in-chunk))
      (vector-push-extend (funcall %outer-fn) %chunks)
      (begin-aggregation (aref %chunks length))
      (incf last)
      (setf %count-in-chunk 0))
    (pass-to-aggregation (aref %chunks last) element)
    (incf %count-in-chunk)))


(defmethod cl-ds.alg.meta:extract-result ((aggregator split-into-chunks-aggregator))
  (bind (((:slots %chunks) aggregator))
    (~> (map 'vector #'cl-ds.alg.meta:extract-result %chunks)
        cl-ds:whole-range)))


(defmethod proxy-range-aggregator-outer-fn ((range split-into-chunks-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (bind ((outer-fn (call-next-method)))
    (if (typep function 'cl-ds.alg.meta:multi-aggregation-function)
        (lambda ()
          (make 'multi-split-into-chunks-aggregator
                :outer-fn outer-fn
                :maximal-count-in-chunk (read-count-in-chunk range)
                :stages (apply #'cl-ds.alg.meta:multi-aggregation-stages
                               function
                               arguments)
                :key key))
        (lambda ()
          (make 'linear-split-into-chunks-aggregator
                :outer-fn outer-fn
                :key key
                :maximal-count-in-chunk (read-count-in-chunk range))))))


(defclass split-into-chunks-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric split-into-chunks (range chunk-size)
  (:generic-function-class split-into-chunks-function)
  (:method (range chunk-size)
    (apply-range-function range #'split-into-chunks :chunk-size chunk-size)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn split-into-chunks-function)
                        &rest all &key chunk-size)
  (declare (ignore all))
  (make-proxy range 'forward-split-into-chunks-proxy
              :count-in-chunk chunk-size))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn split-into-chunks-function)
                        &rest all &key chunk-size)
  (declare (ignore all))
  (make-proxy range 'bidirectional-split-into-chunks-proxy
              :count-in-chunk chunk-size))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn split-into-chunks-function)
                        &rest all &key chunk-size)
  (declare (ignore all))
  (make-proxy range 'random-access-split-into-chunks-proxy
              :count-in-chunk chunk-size))
