(in-package #:cl-data-structures.algorithms)


(defclass restrain-size-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric restrain-size (range size)
  (:generic-function-class restrain-size-function)
  (:method (range size)
    (apply-range-function range
                          #'restrain-size
                          :size size)))


(defclass restrain-size-proxy (proxy-range)
  ((%size :initarg :size
          :reader read-size)
   (%position :initform 0
              :initarg :position
              :accessor access-position)
   (%initial-position :initform 0
                      :initarg :position
                      :reader read-initial-position)))


(defclass forward-restrain-size-proxy (cl-ds:chunking-mixin
                                       restrain-size-proxy
                                       forward-proxy-range)
  ())


(defmethod cl-ds:clone ((range restrain-size-proxy))
  (make (type-of range)
        :original-range (~> range read-original-range cl-ds:clone)
        :position (access-position range)
        :size (read-size range)))


(defmethod cl-ds:consume-front ((range restrain-size-proxy))
  (if (< (access-position range) (read-size range))
      (bind (((:values result more) (~> range
                                        read-original-range
                                        cl-ds:consume-front)))
        (when more
          (incf (access-position range)))
        (values result more))
      (values nil nil)))


(defmethod cl-ds:consume-front ((range restrain-size-proxy))
  (if (< (access-position range) (read-size range))
      (~> range read-original-range cl-ds:peek-front)
      (values nil nil)))


(defmethod cl-ds:reset! ((range restrain-size-proxy))
  (setf (access-position range) (read-initial-position range))
  (call-next-method))


(defmethod cl-ds.alg.meta:apply-layer ((range fundamental-range)
                                       (fn restrain-size-function)
                                       &rest all &key size)
  (declare (ignore all))
  (make 'forward-restrain-size-proxy
        :size size
        :original-range range))
