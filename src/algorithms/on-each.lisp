(in-package #:cl-data-structures.algorithms)


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


(defclass forward-proxy-box-range (forward-proxy-range
                                   proxy-box-range)
  ((%forward-cache :accessor access-forward-cache)))


(defclass bidirectional-proxy-box-range (bidirectional-proxy-range
                                         proxy-box-range)
  ((%backward-cache :accessor access-backward-cache)))


(defclass random-access-proxy-box-range (random-access-proxy-range
                                         bidirectional-proxy-box-range)
  ((%at-cache :reader read-at-cache
              :initarg :at-cache)))


(defclass single-random-access-proxy-box-range (random-access-proxy-box-range)
  ())


(defclass key-value-random-access-proxy-box-range (random-access-proxy-box-range)
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
                :function function)))


(defmethod apply-layer ((range fundamental-range)
                        (fn on-each-function)
                        &rest all &key function)
  (declare (ignore all))
  (on-each-proxy-range-from-range range function))


(defmethod consume-front ((range forward-proxy-box-range))
  (prog1 (with-slots ((cache %forward-cache)) range
           (if (slot-boundp range '%forward-cache)
               (progn (call-next-method) cache)
               (funcall (read-function range)
                        (call-next-method))))
    (slot-makunbound range '%forward-cache)))


(defmethod consume-back ((range bidirectional-proxy-box-range))
  (prog1 (with-slots ((cache %backward-cache)) range
           (if (slot-boundp range '%backward-cache)
               (progn (call-next-method) cache)
               (funcall (read-function range)
                        (call-next-method))))
    (slot-makunbound range '%backward-cache)))


(defmethod peek-front ((range forward-proxy-box-range))
  (with-slots ((cache %forward-cache)) range
    (if (slot-boundp range '%forward-cache)
        cache
        (setf cache (funcall (read-function range)
                             (call-next-method))))))


(defmethod peek-back ((range bidirectional-proxy-box-range))
  (with-slots ((cache %backward-cache)) range
    (if (slot-boundp range '%backward-cache)
        cache
        (setf cache (funcall (read-function range)
                             (call-next-method))))))


(defmethod at ((range random-access-proxy-box-range) location)
  (let ((cache (slot-value range '%at-cache)))
    (mod-bind (container found value)
              (add cache
                   location
                   (delay (lambda ()
                            (~> range
                                read-original-range
                                (at location)
                                (funcall (read-function range) _)))))
      value)))
