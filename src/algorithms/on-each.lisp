(in-package #:cl-data-structures.algorithms)


(defclass on-each-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric on-each (function range &key key)
  (:generic-function-class on-each-function)
  (:method (function (range fundamental-range) &key (key #'identity))
    (apply-range-function range #'on-each :function function :key key)))


(defclass proxy-box-range ()
  ((%function :initarg :function
              :reader read-function)
   (%key :initarg :key
         :reader read-key)))


(defclass forward-proxy-box-range (proxy-box-range forward-proxy-range)
  ())


(defclass bidirectional-proxy-box-range (bidirectional-proxy-range
                                         forward-proxy-box-range)
  ())


(defclass random-access-proxy-box-range (random-access-proxy-range
                                         bidirectional-proxy-box-range)
  ())


(defmethod wrap-chunk ((range forward-proxy-box-range)
                       (chunk cl-ds:fundamental-forward-range))
  (make 'forward-proxy-box-range
        :function (read-function range)
        :key (read-key range)
        :original-range chunk))


(defmethod cl-ds:clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (cl-ds:clone (read-original-range range))
                 :function (read-function range)
                 :key (read-key range)))


(defmethod cl-ds:traverse (function (range forward-proxy-box-range))
  (cl-ds:traverse (compose function (read-function range) (or (read-key range) #'identity))
                  (read-original-range range))
  range)


(defmethod cl-ds:across (function (range forward-proxy-box-range))
  (cl-ds:across (compose function (read-function range) (or (read-key range) #'identity))
                (read-original-range range))
  range)


(defgeneric on-each-proxy-range-from-range (range function key)
  (:method :around ((range fundamental-range) function key)
    (check-type function (or symbol function))
    (check-type key (or symbol function))
    (call-next-method))
  (:method ((range fundamental-forward-range) function key)
    (make 'forward-proxy-box-range
          :original-range range
          :function function
          :key key))
  (:method ((range fundamental-random-access-range) function key)
    (make 'random-access-proxy-box-range
          :function function
          :key key
          :original-range range))
  (:method ((range fundamental-bidirectional-range) function key)
    (make 'bidirectional-proxy-box-range
          :original-range range
          :function function
          :key key)))


(defmethod apply-layer ((range fundamental-range)
                        (fn on-each-function)
                        &rest all &key function key)
  (declare (ignore all))
  (on-each-proxy-range-from-range range function key))


(defmethod cl-ds:consume-front ((range forward-proxy-box-range))
  (bind (((:values elt found) (cl-ds:consume-front (read-original-range range))))
    (if found
        (values (~>> elt
                     (funcall (read-key range))
                     (funcall (read-function range)))
                t)
        (values nil nil))))


(defmethod cl-ds:consume-back ((range bidirectional-proxy-box-range))
  (bind (((:values elt found) (cl-ds:consume-back (read-original-range range))))
    (if found
        (values (~>> elt
                     (funcall (read-key range))
                     (funcall (read-function range)))
                t)
        (values nil nil))))


(defmethod cl-ds:peek-front ((range forward-proxy-box-range))
  (bind (((:values elt found) (peek-front (read-original-range range))))
    (if found
        (values (~>> elt
                     (funcall (read-key range))
                     (funcall (read-function range)))
                t)
        (values nil nil))))


(defmethod cl-ds:peek-back ((range bidirectional-proxy-box-range))
  (bind (((:values elt found) (peek-back (read-original-range range))))
    (if found
        (values (~>> elt
                     (funcall (read-key range))
                     (funcall (read-function range)))
                t)
        (values nil nil))))


(defmethod cl-ds:at ((range bidirectional-proxy-box-range) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (bind (((:values elt found) (cl-ds:at (read-original-range range) location)))
    (if found
        (values (~>> elt
                     (funcall (read-key range))
                     (funcall (read-function range)))
                t)
        (values nil nil))))


(defmethod cl-ds:drop-back ((range bidirectional-proxy-box-range) count)
  (drop-back (read-original-range range) count)
  range)


(defmethod cl-ds:drop-front ((range forward-proxy-box-range) count)
  (drop-front (read-original-range range) count)
  range)
