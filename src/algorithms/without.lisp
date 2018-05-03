(in-package #:cl-data-structures.algorithms)


(defclass without-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%predicate :initarg :predicate
               :reader read-predicate)))


(defclass forward-without-proxy (without-proxy
                                 fundamental-forward-range)
  ())


(defclass bidirectional-without-proxy (forward-without-proxy
                                       fundamental-bidirectional-range)
  ())


(defclass without-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%key :initarg :key
         :reader read-key)
   (%predicate :initarg :predicate
               :reader read-predicate)
   (%outer :initarg :outer
           :reader read-outer)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator without-aggregator)
                                               element)
  (unless (funcall (read-predicate aggregator)
                   (funcall (read-key aggregator) element))
    (cl-ds.alg.meta:pass-to-aggregation (read-outer aggregator)
                                        element)))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator without-aggregator))
  (cl-ds.alg.meta:aggregator-finished-p (read-outer aggregator)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator without-aggregator))
  (cl-ds.alg.meta:end-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator without-aggregator))
  (cl-ds.alg.meta:begin-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:extract-result ((aggregator without-aggregator))
  (cl-ds.alg.meta:extract-result (read-outer aggregator)))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator without-aggregator))
  (cl-ds.alg.meta:expects-content-p (read-outer aggregator)))


(defmethod cl-ds:peek-front ((aggregator forward-without-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:peek-front outer))
      (when (null more)
        (return (values nil nil)))
      (if (predicate (key value))
          (cl-ds:consume-front outer)
          (leave (values value more))))))


(defmethod cl-ds:consume-front ((aggregator forward-without-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:consume-front outer))
      (when (null more)
        (return (values nil nil)))
      (unless (predicate (key value))
        (leave (values value more))))))


(defmethod cl-ds:peek-back ((aggregator forward-without-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:peek-back outer))
      (when (null more)
        (return (values nil nil)))
      (if (predicate (key value))
          (cl-ds:consume-back outer)
          (leave (values value more))))))


(defmethod cl-ds:consume-back ((aggregator bidirectional-without-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:consume-back outer))
      (when (null more)
        (return (values nil nil)))
      (unless (predicate (key value))
        (leave (values value more))))))


(defmethod cl-ds.alg.meta:construct-aggregator ((range without-proxy)
                                                key
                                                (function cl-ds.alg.meta:aggregation-function)
                                                outer-fn
                                                (arguments list))
  (make 'without-aggregator :key (read-key range)
                            :predicate (read-predicate range)
                            :outer (funcall (proxy-range-aggregator-outer-fn
                                             range
                                             key
                                             function
                                             outer-fn
                                             arguments))))


(defmethod clone ((range without-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :predicate (read-predicate range)
                 :key (read-key range)))


(defmethod cl-ds:traverse (function (range without-proxy))
  (let ((range (read-original-range range))
        (predicate (read-predicate range))
        (key (read-key range)))
    (cl-ds:traverse (lambda (x) (unless (funcall predicate (funcall key x))
                                  (funcall function x)))
                    range)
    range))


(defmethod cl-ds:across (function (range without-proxy))
  (let ((range (read-original-range range))
        (predicate (read-predicate range))
        (key (read-key range)))
    (cl-ds:across (lambda (x) (unless (funcall predicate (funcall key x))
                                (funcall function x)))
                  range)
    range))


(defclass without-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric without (predicate range &key key)
  (:generic-function-class without-function)
  (:method (predicate range &key (key #'identity))
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
