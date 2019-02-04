(in-package #:cl-data-structures.algorithms)


(defclass without-proxy (filtering-proxy)
  ((%predicate :initarg :predicate
               :reader read-predicate)))


(defclass forward-without-proxy (without-proxy
                                 forward-filtering-proxy)
  ())


(defclass bidirectional-without-proxy (forward-without-proxy
                                       bidirectional-filtering-proxy)
  ())


(defmethod should-skip ((range without-proxy) element can-mutate)
  (declare (ignore can-mutate))
  (funcall (read-predicate range) element))


(defclass without-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod clone ((range without-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :predicate (read-predicate range)
                 :key (read-key range)))


(defgeneric without (range predicate &key key)
  (:generic-function-class without-function)
  (:method (range predicate &key (key #'identity))
    (apply-range-function range #'without :key key :predicate predicate)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function without-function)
                        &rest all &key predicate key)
  (declare (ignore all))
  (make 'bidirectional-without-proxy
        :predicate predicate
        :key key
        :original-range range))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function without-function)
                        &rest all &key predicate key)
  (declare (ignore all))
  (make 'forward-without-proxy
        :predicate predicate
        :key key
        :original-range range))


(defmethod wrap-chunk ((range without-proxy)
                       (chunk cl-ds:fundamental-forward-range))
  (without chunk (read-predicate range) :key (read-key range)))
