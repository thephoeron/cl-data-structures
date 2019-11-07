(in-package #:cl-data-structures.algorithms)


(defclass flatten-proxy (cl-ds:chunking-mixin
                         proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%current :initform nil
             :initarg :current
             :accessor access-current)))


(defmethod cl-ds.utils:cloning-information append
    ((range flatten-proxy))
  '((:key read-key)
    (:current access-current)))


(defclass forward-flatten-proxy (flatten-proxy
                                 fundamental-forward-range)
  ())


(labels ((impl (function x)
           (labels ((inner (x)
                      (if (listp x)
                          (map nil #'inner x)
                          (funcall function x))))
             (inner x))))
  (defmethod cl-ds:traverse ((range flatten-proxy) function)
    (cl-ds:traverse (read-original-range range)
                    (compose (curry #'impl function) (read-key range)))
    range)


  (defmethod cl-ds:across ((range flatten-proxy) function)
    (cl-ds:across (read-original-range range)
                  (compose (curry #'impl function) (read-key range)))
    range))


(defmethod cl-ds:reset! ((range flatten-proxy))
  (setf (access-current range) nil)
  (call-next-method))


(defmethod cl-ds:peek-front ((range forward-flatten-proxy))
  (bind ((current (access-current range))
         ((:labels value (element))
          (cond ((null element) nil)
                ((atom element) element)
                ((listp element)
                 (iterate
                   (for elt in element)
                   (for result = (value elt))
                   (until result)
                   (finally (return result))))))
         (result (value current)))
    (if (null result)
        (~> range cl-ds:clone cl-ds:consume-front)
        (values result t))))


(defmethod cl-ds:consume-front ((range forward-flatten-proxy))
  (bind ((key (read-key range))
         ((:accessors (current access-current)) range)
         ((:labels value ())
          (cond ((null current) nil)
                ((atom current) (shiftf current nil))
                ((listp current)
                 (iterate
                   (for elt = (first current))
                   (setf current (rest current))
                   (cond ((listp elt)
                          (setf current (append elt current)))
                         ((atom elt)
                          (leave elt)))
                   (until (null current))))))
         (result (value)))
    (if (null result)
        (iterate
          (with outer = (read-original-range range))
          (for (values f-val more) = (cl-ds:consume-front outer))
          (when (not more)
            (leave (values nil nil)))
          (setf current (funcall key f-val))
          (for result = (value))
          (unless (null result)
            (leave (values result t))))
        (values result t))))


(defclass flatten-lists-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric flatten-lists (range &key key)
  (:generic-function-class flatten-lists-function)
  (:method (range &key (key #'identity))
    (ensure-functionf key)
    (apply-range-function range #'flatten-lists :key key)))


(defmethod apply-layer ((range traversable)
                        (function flatten-lists-function)
                        &rest all &key key)
  (declare (ignore all))
  (make 'forward-flatten-proxy
        :key key
        :original-range range))


(defclass flatten-lists-aggregator (cl-ds.alg.meta:abstract-proxy-aggregator)
  ((%flatten-key :initarg :flatten-key
                  :reader read-flatten-key)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator flatten-lists-aggregator)
                                               element)
  (bind (((:slots %flatten-key) aggregator)
         (inner-aggregator (cl-ds.alg.meta:read-inner-aggregator aggregator))
         (selected (~>> element (funcall %flatten-key)))
         ((:labels impl (data))
          (if (atom data)
              (cl-ds.alg.meta:pass-to-aggregation inner-aggregator
                                                  data)
              (map nil #'impl data))))
    (impl selected)))


(defmethod proxy-range-aggregator-outer-fn ((range flatten-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (let ((flatten-key (read-key range)))
    (lambda ()
      (make 'flatten-lists-aggregator
            :key key
            :flatten-key flatten-key
            :inner-aggregator (funcall (call-next-method))))))


(defmethod cl-ds.alg.meta:across-aggregate ((range flatten-proxy) function)
  (~> range
      read-original-range
      (cl-ds.alg.meta:across-aggregate function)))
