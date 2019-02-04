(in-package #:cl-data-structures.algorithms)


(defclass only-proxy (filtering-proxy)
  ((%predicate :initarg :predicate
               :reader read-predicate)))


(defclass forward-only-proxy (only-proxy
                              forward-filtering-proxy)
  ())


(defclass bidirectional-only-proxy (forward-only-proxy
                                    bidirectional-filtering-proxy)
  ())


(defmethod clone ((range only-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :predicate (read-predicate range)
                 :key (read-key range)))


(defmethod should-skip ((range only-proxy) element)
  (~>> element (funcall (read-predicate range)) not))


(defclass only-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod wrap-chunk ((range forward-only-proxy)
                       (chunk cl-ds:fundamental-forward-range))
  (make 'forward-only-proxy
        :original-range chunk
        :key (read-key range)
        :predicate (read-predicate range)))


(defgeneric only (range predicate &key key)
  (:generic-function-class only-function)
  (:method (range predicate &key (key #'identity))
    (apply-range-function range #'only :key key :predicate predicate)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function only-function)
                        &rest all &key predicate key)
  (declare (ignore all))
  (make 'bidirectional-only-proxy
        :predicate predicate
        :key key
        :original-range range))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function only-function)
                        &rest all &key predicate key)
  (declare (ignore all))
  (make 'forward-only-proxy
        :predicate predicate
        :key key
        :original-range range))
