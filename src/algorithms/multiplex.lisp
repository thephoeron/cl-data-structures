(in-package #:cl-data-structures.algorithms)


(defclass multiplex-proxy (cl-ds:chunking-mixin
                           proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%function :initarg :function
              :reader read-function)
   (%current :initform nil
             :initarg :current
             :accessor access-current)
   (%initial-current :initform nil
                     :initarg :initial-current
                     :reader read-initial-current)))


(defmethod cl-ds.utils:cloning-information append
    ((range multiplex-proxy))
  '((:key read-key)
    (:current access-current)
    (:function read-function)))


(defmethod cl-ds:clone ((range multiplex-proxy))
  (bind (((:slots %current) range))
    (make (class-of range)
          :current #1=(if (null %current)
                          nil
                          (cl-ds:clone %current))
          :initial-current #1#
          :function (read-function range)
          :key (read-key range)
          :original-range (~> range
                               read-original-range
                               cl-ds:clone))))


(defmethod cl-ds:traverse ((range multiplex-proxy) function)
  (ensure-functionf function)
  (bind ((key (read-key range))
         (range-function (read-function range))
         ((:slots %current) range))
    (cl-ds:traverse (read-original-range range)
                    (lambda (x &aux (elt (funcall key x)))
                      (cl-ds:traverse %current function)
                      (setf %current (funcall range-function elt))
                      (cl-ds:traverse %current function)
                      (setf %current nil)))
    range))


(defmethod cl-ds:across ((range multiplex-proxy) function)
  (cl-ds:traverse (cl-ds:clone range) function)
  range)


(defmethod cl-ds:reset! ((range multiplex-proxy))
  (setf (access-current range) (let ((current (read-initial-current range)))
                                 (if (null current)
                                     nil
                                     (cl-ds:clone current))))
  (call-next-method)
  range)


(defmethod cl-ds:consume-front ((range multiplex-proxy))
  (bind (((:slots %current) range)
         (function (read-function range))
         (key (read-key range))
         (inner-range (read-original-range range)))
    (iterate
      (for (values element more) = (cl-ds:consume-front %current))
      (when more
        (return-from cl-ds:consume-front (values element more)))
      (setf %current nil)
      (for (values next even-more) = (cl-ds:consume-front inner-range))
      (unless even-more
        (return-from cl-ds:consume-front (values nil nil)))
      (setf %current (~>> next (funcall key) (funcall function))))
    range))


(defmethod cl-ds:peek-front ((range multiplex-proxy))
  (~> range cl-ds:clone cl-ds:consume-front))


(defclass multiplex-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric multiplex (range &key key function)
  (:generic-function-class multiplex-function)
  (:method (range &key (key #'identity) (function #'cl-ds:whole-range))
    (ensure-functionf key)
    (apply-range-function range #'multiplex :key key :function function)))


(defclass forward-multiplex-proxy (multiplex-proxy
                                   cl-ds:fundamental-forward-range)
  ())


(defmethod apply-layer ((range traversable)
                        (fn multiplex-function)
                        &rest all &key key function)
  (declare (ignore all))
  (make 'forward-multiplex-proxy
        :original-range range
        :key key
        :function function))


(defclass multiplex-aggregator (cl-ds.alg.meta:abstract-proxy-aggregator)
  ((%range :initarg :range
           :reader read-range)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator multiplex-aggregator)
                                               element)
  (bind ((range (read-range aggregator))
         (function (read-function range))
         (inner-aggregator (cl-ds.alg.meta:read-inner-aggregator aggregator))
         (key (read-key range)))
    (~> element
        (funcall key _)
        (funcall function _)
        (cl-ds:traverse (lambda (x)
                          (cl-ds.alg.meta:pass-to-aggregation
                           inner-aggregator
                           x))))))


(defmethod proxy-range-aggregator-outer-fn ((range multiplex-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (lambda ()
    (make 'multiplex-aggregator
          :key key
          :range range
          :inner-aggregator (funcall (call-next-method)))))


(defmethod cl-ds.alg.meta:across-aggregate ((range multiplex-proxy) function)
  (~> range
      read-original-range
      (cl-ds.alg.meta:across-aggregate function)))
