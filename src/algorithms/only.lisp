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


(defmethod cl-ds:peek-front ((aggregator forward-only-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-original-range aggregator))
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
      (with outer = (read-original-range aggregator))
      (for (values value more) = (cl-ds:consume-front outer))
      (when (null more)
        (return (values nil nil)))
      (when (predicate (key value))
        (leave (values value more))))))


(defmethod cl-ds:peek-back ((aggregator forward-only-proxy))
  (fbind ((key (read-key aggregator))
          (predicate (read-predicate aggregator)))
    (iterate
      (with outer = (read-original-range aggregator))
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
      (with outer = (read-original-range aggregator))
      (for (values value more) = (cl-ds:consume-back outer))
      (when (null more)
        (return (values nil nil)))
      (unless (predicate (key value))
        (leave (values value more))))))


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
