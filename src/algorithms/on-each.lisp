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
              :reader read-function)
   (%funcall-result :initarg :funcall-result
                    :reader read-funcall-result)))


(defmethod initialize-instance :after ((range proxy-box-range)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (when (slot-boundp range '%funcall-result)
    (let ((cache (make-instance 'flexichain:standard-flexichain)))
      (cl-ds:traverse (compose (curry #'flexichain:push-end
                                      cache)
                               (curry #'funcall
                                      (read-function range)))
                      (read-original-range range))
      (setf (slot-value range '%funcall-result) cache))))


(defclass forward-proxy-box-range (forward-proxy-range
                                   proxy-box-range)
  ())


(defclass bidirectional-proxy-box-range (bidirectional-proxy-range
                                         proxy-box-range)
  ())


(defmethod clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (read-original-range range)
                 :funcall-result (read-funcall-result range)
                 :function (read-function range)))


(defmethod cl-ds:traverse (function (range forward-proxy-box-range))
  (iterate
    (with cache = (read-funcall-result range))
    (for i from 0 below (flexichain:nb-elements cache))
    (funcall function (flexichain:element* cache i))
    (finally (return range))))


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
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (values (flexichain:pop-start cache) t))))


(defmethod consume-back ((range bidirectional-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (values (flexichain:pop-end cache) t))))


(defmethod peek-front ((range forward-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (let ((result (flexichain:pop-start cache)))
          (flexichain:push-start cache result)
          (values result t)))))


(defmethod peek-back ((range bidirectional-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (let ((result (flexichain:pop-end cache)))
          (flexichain:push-end cache result)
          (values result t)))))


(defmethod cl-ds:morep ((range proxy-box-range))
  (not (flexichain:flexi-empty-p (read-funcall-result range))))
