(in-package #:cl-data-structures.algorithms)


(defclass without-proxy (filtering-proxy)
  ((%predicate :initarg :predicate
               :reader read-predicate)))


(defmethod cl-ds.utils:cloning-information append
    ((proxy without-proxy))
  '((:predicate read-predicate)))


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


(defgeneric without (range predicate &key key)
  (:generic-function-class without-function)
  (:method (range predicate &key (key #'identity))
    (apply-range-function range #'without
                          (list range predicate
                                :key key))))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function without-function)
                        all)
  (make 'bidirectional-without-proxy
        :predicate (second all)
        :key (cl-ds.utils:at-list all :key)
        :original-range range))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function without-function)
                        all)
  (make 'forward-without-proxy
        :predicate (second all)
        :key (cl-ds.utils:at-list all :key)
        :original-range range))


(defmethod wrap-chunk ((range without-proxy)
                       (chunk cl-ds:fundamental-forward-range))
  (without chunk (read-predicate range) :key (read-key range)))
