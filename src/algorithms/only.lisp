(cl:in-package #:cl-data-structures.algorithms)


(defclass only-proxy (filtering-proxy)
  ((%predicate :initarg :predicate
               :reader read-predicate)))


(defmethod cl-ds.utils:cloning-information append
    ((proxy only-proxy))
  '((:predicate read-predicate)))


(defclass forward-only-proxy (only-proxy
                              forward-filtering-proxy)
  ())


(defclass bidirectional-only-proxy (forward-only-proxy
                                    bidirectional-filtering-proxy)
  ())


(defmethod should-skip ((range only-proxy) element can-mutate)
  (declare (ignore can-mutate))
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
    (apply-range-function range #'only
                          (list range predicate
                                :key key))))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function only-function)
                        all)
  (make 'bidirectional-only-proxy
        :predicate (second all)
        :key (cl-ds.utils:at-list all :key)
        :original-range range))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function only-function)
                        all)
  (make 'forward-only-proxy
        :predicate (second all)
        :key (cl-ds.utils:at-list all :key)
        :original-range range
        ))
