(in-package #:cl-data-structures.algorithms)


(defclass flatten-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%current :initform nil
             :initarg :current
             :accessor access-current)))


(defclass forward-flatten-proxy (flatten-proxy
                                 fundamental-forward-range)
  ())


(defmethod cl-ds:clone ((range flatten-proxy))
  (make (type-of range)
        :key (read-key range)
        :current (access-current range)
        :original-range (read-original-range range)))


(labels ((impl (function x)
           (labels ((impl (x)
                      (if (listp x)
                          (map nil #'impl x)
                          (funcall function x))))
             (impl x))))
  (defmethod cl-ds:traverse (function (range flatten-proxy))
    (cl-ds:traverse (compose (curry function #'impl) (read-key range))
                    (read-original-range range))
    range)


  (defmethod cl-ds:across (function (range flatten-proxy))
    (cl-ds:across (compose (curry function #'impl) (read-key range))
                  (read-original-range range))
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
          (setf current (funcall key value))
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


(defmethod apply-layer ((range fundamental-forward-range)
                        (function flatten-lists-function)
                        &rest all &key key)
  (declare (ignore all))
  (make 'forward-flatten-proxy
        :key key
        :original-range range))
