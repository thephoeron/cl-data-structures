(cl:in-package #:cl-data-structures.algorithms)


(defclass filtering-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)))


(defmethod cl-ds.utils:cloning-information append
    ((range filtering-proxy))
  '((:key read-key)))


(defclass forward-filtering-proxy (filtering-proxy
                                   fundamental-forward-range)
  ())


(defclass bidirectional-filtering-proxy (filtering-proxy
                                         fundamental-bidirectional-range)
  ())


(defgeneric should-skip (range element can-mutate))


(defmethod cl-ds:peek-front ((range forward-filtering-proxy))
  (iterate
    (with outer = (read-original-range range))
    (with key = (read-key range))
    (for (values value more) = (cl-ds:peek-front outer))
    (when (null more)
      (return (values nil nil)))
    (if (should-skip range (funcall key value) nil)
        (cl-ds:consume-front outer)
        (leave (values value more)))))


(defmethod cl-ds:consume-front ((range forward-filtering-proxy))
  (iterate
    (with key = (read-key range))
    (with outer = (read-original-range range))
    (for (values value more) = (cl-ds:consume-front outer))
    (when (null more)
      (return (values nil nil)))
    (unless (should-skip range (funcall key value) t)
      (leave (values value more)))))


(defmethod cl-ds:peek-back ((range bidirectional-filtering-proxy))
  (iterate
    (with key = (read-key range))
    (with outer = (read-original-range range))
    (for (values value more) = (cl-ds:peek-back outer))
    (when (null more)
      (return (values nil nil)))
    (if (should-skip range (funcall key value) nil)
        (cl-ds:consume-back outer)
        (leave (values value more)))))


(defmethod cl-ds:consume-back ((range bidirectional-filtering-proxy))
  (iterate
    (with key = (read-key range))
    (with outer = (read-original-range range))
    (for (values value more) = (cl-ds:consume-back outer))
    (when (null more)
      (return (values nil nil)))
    (unless (should-skip range (funcall key value) t)
      (leave (values value more)))))


(defmethod clone ((range filtering-proxy))
  (cl-ds.utils:quasi-clone* range
    :original-range (clone (read-original-range range))))


(defmethod cl-ds:traverse ((range filtering-proxy) function)
  (let ((key (read-key range)))
    (cl-ds:traverse (read-original-range range)
                    (lambda (x) (unless (should-skip range
                                                     (funcall key x)
                                                     t)
                                  (funcall function x))))
    range))


(defmethod cl-ds:across ((range filtering-proxy) function)
  (~> range cl-ds:clone (cl-ds:traverse function))
  range)


(defmethod cl-ds.alg.meta:aggregator-constructor ((range filtering-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let ((outer-fn (call-next-method))
        (key (ensure-function (read-key range))))
    (cl-ds.utils:cases ((:variant (eq key #'identity)))
      (cl-ds.alg.meta:aggregator-constructor
       (read-original-range range)
       (cl-ds.alg.meta:let-aggregator ((inner (cl-ds.alg.meta:call-constructor outer-fn)))
           ((element)
             (unless (should-skip range range (funcall key element))
               (cl-ds.alg.meta:pass-to-aggregation inner element)))

           ((cl-ds.alg.meta:extract-result inner)))
       function
       arguments))))
