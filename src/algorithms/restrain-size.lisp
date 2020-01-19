(cl:in-package #:cl-data-structures.algorithms)


(defclass restrain-size-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric restrain-size (range size)
  (:generic-function-class restrain-size-function)
  (:method (range size)
    (apply-range-function range #'restrain-size
                          (list range
                                #'restrain-size
                                :size size))))


(defclass restrain-size-proxy (proxy-range)
  ((%size :initarg :size
          :reader read-size)
   (%position :initform 0
              :initarg :position
              :accessor access-position)
   (%initial-position :initform 0
                      :initarg :position
                      :reader read-initial-position)))


(defmethod cl-ds.utils:cloning-information append
    ((range restrain-size-proxy))
  '((:size read-size)
    (:position access-position)))


(defclass forward-restrain-size-proxy (cl-ds:chunking-mixin
                                       restrain-size-proxy
                                       forward-proxy-range)
  ())


(defmethod cl-ds:consume-front ((range restrain-size-proxy))
  (if (< (access-position range) (read-size range))
      (bind (((:values result more) (~> range
                                        read-original-range
                                        cl-ds:consume-front)))
        (when more
          (incf (access-position range)))
        (values result more))
      (values nil nil)))


(defmethod cl-ds:peek-front ((range restrain-size-proxy))
  (if (< (access-position range) (read-size range))
      (~> range read-original-range cl-ds:peek-front)
      (values nil nil)))


(defmethod cl-ds:reset! ((range restrain-size-proxy))
  (setf (access-position range) (read-initial-position range))
  (call-next-method))


(defmethod cl-ds.alg.meta:apply-layer ((range fundamental-range)
                                       (fn restrain-size-function)
                                       all)
  (let ((size (cl-ds.utils:at-list all :size)))
    (check-type size integer)
    (unless (<= 0 size)
      (error 'cl-ds:argument-out-of-bounds
             :argument 'size
             :bounds '(< 0)
             :value size))
    (make 'forward-restrain-size-proxy
          :size size
          :original-range range)))


(defmethod cl-ds:across ((range restrain-size-proxy) function)
  (iterate
    (with og-range = (~> range read-original-range cl-ds:clone))
    (for i from (access-position range) below (read-size range))
    (for (values data more) = (cl-ds:consume-front og-range))
    (while more)
    (funcall function data))
  range)


(defmethod cl-ds:traverse ((range restrain-size-proxy) function)
  (iterate
    (with og-range = (~> range read-original-range))
    (for i from (access-position range) below (read-size range))
    (for (values data more) = (cl-ds:consume-front og-range))
    (while more)
    (incf (access-position range))
    (funcall function data))
  range)
