(in-package #:cl-data-structures)


(defclass on-each-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric on-each (function range)
  (:generic-function-class on-each-function)
  (:method (function (range fundamental-range))
    (apply-range-function range #'on-each :function function)))


(defclass proxy-box-range ()
  ((%function :initarg :function
              :reader read-function)))


(defclass forward-proxy-box-range (forward-proxy-range proxy-box-range)
  ())


(defclass bidirectional-proxy-box-range (bidirectional-proxy-range proxy-box-range)
  ())


(defclass random-access-proxy-box-range (random-access-proxy-range proxy-box-range)
  ())


(defmethod clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :function (read-function range)))


(defgeneric on-each-proxy-range-from-range (range function)
  (:method :around ((range fundamental-range) function)
    (check-type function (or symbol function))
    (call-next-method))
  (:method ((range fundamental-forward-range) function)
    (make-proxy range 'forward-proxy-box-range
                :function function))
  (:method ((range fundamental-bidirectional-range) function)
    (make-proxy range 'bidirectional-proxy-box-range
                :function function))
  (:method ((range fundamental-random-access-range) function)
    (make-proxy range 'random-access-proxy-box-range
                :function function)))


(defmethod apply-layer ((range fundamental-range)
                        (fn on-each-function)
                        &rest all &key function)
  (declare (ignore all))
  (on-each-proxy-range-from-range range function))

(defmethod consume-front ((range forward-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod consume-back ((range bidirectional-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod peek-front ((range forward-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod peek-back ((range bidirectional-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod at ((range random-access-proxy-box-range) location)
  (funcall (read-function range) (call-next-method)))

