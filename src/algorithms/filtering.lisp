(in-package #:cl-data-structures.algorithms)


(defclass filtering-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)))


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
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :key (read-key range)))


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
