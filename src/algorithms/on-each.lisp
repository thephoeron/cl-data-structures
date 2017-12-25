(in-package #:cl-data-structures.algorithms)


(defclass on-each-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric on-each (function range &key key)
  (:generic-function-class on-each-function)
  (:method (function (range fundamental-range) &key key)
    (apply-range-function range #'on-each :function function :key key)))


(defclass proxy-box-range ()
  ((%function :initarg :function
              :reader read-function)
   (%funcall-result :initarg :funcall-result
                    :reader read-funcall-result)
   (%key :initarg :key
         :reader read-key)))


(defmethod initialize-instance :after ((range proxy-box-range)
                                       &rest initargs
                                       &key &allow-other-keys)
  (declare (ignore initargs))
  (unless (slot-boundp range '%funcall-result)
    (let ((cache (make-instance 'flexichain:standard-flexichain))
          (function (read-function range))
          (key (read-key range)))
      (cl-ds:traverse (compose (curry #'flexichain:push-end cache)
                               (if (null key)
                                   (curry #'funcall function)
                                   (lambda (x)
                                     (funcall function (funcall key x)))))
                      (read-original-range range))
      (setf (slot-value range '%funcall-result) cache))))


(defclass forward-proxy-box-range (forward-proxy-range
                                   proxy-box-range)
  ())


(defclass bidirectional-proxy-box-range (bidirectional-proxy-range
                                         proxy-box-range)
  ())


(defmethod cl-ds:clone ((range forward-proxy-box-range))
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


(defgeneric on-each-proxy-range-from-range (range function key)
  (:method :around ((range fundamental-range) function key)
    (check-type function (or symbol function))
    (check-type key (or symbol function))
    (call-next-method))
  (:method ((range fundamental-forward-range) function key)
    (make-proxy range 'forward-proxy-box-range
                :function function
                :key key))
  (:method ((range fundamental-bidirectional-range) function key)
    (make-proxy range 'bidirectional-proxy-box-range
                :function function
                :key key)))


(defmethod cl-ds:apply-layer ((range fundamental-range)
                              (fn on-each-function)
                              &rest all &key function key)
  (declare (ignore all))
  (on-each-proxy-range-from-range range function key))


(defmethod cl-ds:consume-front ((range forward-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (values (flexichain:pop-start cache) t))))


(defmethod cl-ds:consume-back ((range bidirectional-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (values (flexichain:pop-end cache) t))))


(defmethod cl-ds:peek-front ((range forward-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (let ((result (flexichain:pop-start cache)))
          (flexichain:push-start cache result)
          (values result t)))))


(defmethod cl-ds:peek-back ((range bidirectional-proxy-box-range))
  (let ((cache (read-funcall-result range)))
    (if (flexichain:flexi-empty-p cache)
        (values nil nil)
        (let ((result (flexichain:pop-end cache)))
          (flexichain:push-end cache result)
          (values result t)))))


(defmethod cl-ds:drop-back ((range bidirectional-proxy-box-range) count)
  (let ((cache (read-funcall-result range)))
    (iterate
      (until (flexichain:flexi-empty-p cache))
      (repeat count)
      (flexichain:pop-end cache))
    range))


(defmethod cl-ds:drop-front ((range forward-proxy-box-range) count)
  (let ((cache (read-funcall-result range)))
    (iterate
      (until (flexichain:flexi-empty-p cache))
      (repeat count)
      (flexichain:pop-start cache))
    range))


(defmethod cl-ds:morep ((range proxy-box-range))
  (~> range read-funcall-result flexichain:flexi-empty-p not))
