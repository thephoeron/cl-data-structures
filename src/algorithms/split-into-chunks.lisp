(in-package #:cl-data-structures.algorithms)


(defclass split-into-chunks-proxy (proxy-range)
  ((%count-in-chunk :initarg :count-in-chunk
                    :accessor access-count-in-chunk)))


(defmethod initialize-instance :after ((object split-into-chunks-proxy) &key)
  (check-type (access-count-in-chunk object) integer)
  (unless (< 0 (access-count-in-chunk object))
    (error 'cl-ds:argument-out-of-bounds
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


(defclass split-into-chunks-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%chunks :initform (vect)
            :initarg :chunks
            :type vector
            :reader read-chunks)
   (%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%maximal-count-in-chunk :initarg :maximal-count-in-chunk
                            :reader read-maximal-count-in-chunk)
   (%current-index :initform 0
                   :accessor access-current-index)
   (%count-in-chunk :initarg :count-in-chunk
                    :initform 0
                    :accessor access-count-in-chunk)))


(defclass linear-split-into-chunks-aggregator (split-into-chunks-aggregator)
  ())


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator split-into-chunks-aggregator)
                                               element)
  (bind (((:slots %chunks %maximal-count-in-chunk
                  %outer-fn %current-index %count-in-chunk)
          aggregator))
    (if (emptyp %chunks)
        (progn (vector-push-extend (funcall %outer-fn) %chunks)
               (begin-aggregation (last-elt %chunks)))
        (when (>= %count-in-chunk %maximal-count-in-chunk)
          (incf %current-index)
          (when (>= %current-index (length %chunks))
            (vector-push-extend (funcall %outer-fn) %chunks)
            (begin-aggregation (last-elt %chunks)))
          (setf %count-in-chunk 0)))
    (~> (aref %chunks %current-index) (pass-to-aggregation element))
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
    (lambda ()
      (make 'linear-split-into-chunks-aggregator
            :outer-fn outer-fn
            :key key
            :maximal-count-in-chunk (access-count-in-chunk range)))))


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
