(in-package #:cl-data-structures)


(defclass on-each-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric on-each (function range)
  (:generic-function-class on-each-function)
  (:method (function (range fundamental-range))
    (apply-range-function range #'on-each :function function)))


(defclass forward-proxy-box-range (fundamental-forward-range)
  ((%original-range :initarg :original-range
                    :reader read-original-range)
   (%function :initarg :function
              :reader read-function)))


(defmethod clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :function (read-function range)))


(defclass bidirectional-proxy-box-range (forward-proxy-box-range)
  ())


(defclass random-access-proxy-box-range (bidirectional-proxy-box-range)
  ())


(defgeneric on-each-proxy-range-from-range (range function)
  (:method :around ((range fundamental-range) function)
    (check-type function (or symbol function))
    (call-next-method))
  (:method ((range fundamental-forward-range) function)
    (make-instance 'forward-proxy-box-range
                   :original-range range
                   :function function))
  (:method ((range fundamental-bidirectional-range) function)
    (make-instance 'bidirectional-proxy-box-range
                   :original-range range
                   :function function))
  (:method ((range fundamental-random-access-range) function)
    (make-instance 'random-access-proxy-box-range
                   :original-range range
                   :function function)))


(defmethod apply-layer ((range fundamental-range)
                        (fn on-each-function)
                        &rest all &key function)
  (on-each-proxy-range-from-range range function))


(defgeneric change-each! (function range)
  (:generic-function-class transformation!-function))
