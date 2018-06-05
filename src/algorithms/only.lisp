(in-package #:cl-data-structures.algorithms)


(defclass only-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%predicate :initarg :predicate
               :reader read-predicate)))


(defclass forward-only-proxy (only-proxy
                              fundamental-forward-range)
  ())


(defclass bidirectional-only-proxy (forward-only-proxy
                                    fundamental-bidirectional-range)
  ())


(defclass only-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%key :initarg :key
         :reader read-key)
   (%predicate :initarg :predicate
               :reader read-predicate)
   (%outer :initarg :outer
           :reader read-outer)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator only-aggregator)
                                               element)
  (unless (funcall (read-predicate aggregator)
                   (funcall (read-key aggregator) element))
    (cl-ds.alg.meta:pass-to-aggregation (read-outer aggregator)
                                        element)))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator only-aggregator))
  (cl-ds.alg.meta:aggregator-finished-p (read-outer aggregator)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator only-aggregator))
  (cl-ds.alg.meta:end-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator only-aggregator))
  (cl-ds.alg.meta:begin-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:extract-result ((aggregator only-aggregator))
  (cl-ds.alg.meta:extract-result (read-outer aggregator)))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator only-aggregator))
  (cl-ds.alg.meta:expects-content-p (read-outer aggregator)))


(defmethod cl-ds:peek-front ((aggregator forward-only-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:peek-front outer))
      (when (null more)
        (return (values nil nil)))
      (if (predicate (key value))
          (leave (values value more))
          (cl-ds:consume-front outer)))))


(defmethod cl-ds:consume-front ((aggregator forward-only-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:consume-front outer))
      (when (null more)
        (return (values nil nil)))
      (when (predicate (key value))
        (leave (values value more))))))


(defmethod cl-ds:peek-back ((aggregator forward-only-proxy))
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


(defmethod cl-ds:consume-back ((aggregator bidirectional-only-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-outer aggregator))
      (for (values value more) = (cl-ds:consume-back outer))
      (when (null more)
        (return (values nil nil)))
      (unless (predicate (key value))
        (leave (values value more))))))


(defmethod cl-ds.alg.meta:construct-aggregator ((range only-proxy)
                                                key
                                                (function cl-ds.alg.meta:aggregation-function)
                                                outer-fn
                                                (arguments list))
  (make 'only-aggregator :key (read-key range)
                            :predicate (read-predicate range)
                            :outer (funcall (proxy-range-aggregator-outer-fn
                                             range
                                             key
                                             function
                                             outer-fn
                                             arguments))))


(defmethod clone ((range only-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :predicate (read-predicate range)
                 :key (read-key range)))


(defmethod cl-ds:traverse (function (range only-proxy))
  (let ((range (read-original-range range))
        (predicate (read-predicate range))
        (key (read-key range)))
    (cl-ds:traverse (lambda (x)
                      (when (funcall predicate (funcall key x))
                        (funcall function x)))
                    range)
    range))


(defmethod cl-ds:across (function (range only-proxy))
  (let ((range (read-original-range range))
        (predicate (read-predicate range))
        (key (read-key range)))
    (cl-ds:across (lambda (x)
                    (when (funcall predicate (funcall key x))
                      (funcall function x)))
                  range)
    range))


(defclass only-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


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
