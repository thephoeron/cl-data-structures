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


(defmethod cl-ds:peek-front ((range forward-without-proxy))
  (fbind ((key (read-key range))
          (predicate (read-predicate range)))
    (iterate
      (with outer = (read-outer range))
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
