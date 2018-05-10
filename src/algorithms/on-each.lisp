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


(defclass bidirectional-proxy-box-range (proxy-box-range bidirectional-proxy-range)
  ())


(defclass random-access-proxy-box-range (proxy-box-range random-access-proxy-range)
  ())


(defclass proxy-box-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%function :initarg :function
              :reader read-function)
   (%outer :initarg :outer
           :reader read-outer)))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator proxy-box-aggregator))
  (cl-ds.alg.meta:aggregator-finished-p (read-outer aggregator)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator proxy-box-aggregator))
  (cl-ds.alg.meta:begin-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator proxy-box-aggregator))
  (cl-ds.alg.meta:end-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator proxy-box-aggregator))
  (cl-ds.alg.meta:expects-content-p (read-outer aggregator)))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator proxy-box-aggregator)
                                               element)
  (bind (((:slots (%key cl-ds.alg.meta:%key) %outer %function) aggregator)
         (element (~>> element (funcall %key) (funcall %function))))
    (cl-ds.alg.meta:pass-to-aggregation %outer element)))


(defmethod cl-ds.alg.meta:extract-result ((aggregator proxy-box-aggregator))
  (cl-ds.alg.meta:extract-result (read-outer aggregator)))


(defmethod proxy-range-aggregator-outer-fn ((range proxy-box-range)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (bind (((:slots (%key cl-ds.alg.meta:%key) %function) range)
         (outer-fn (call-next-method)))
    (lambda ()
      (let ((outer (funcall outer-fn)))
        (make 'proxy-box-aggregator
              :function %function
              :key %key
              :outer outer)))))


(defmethod cl-ds.alg.meta:construct-aggregator
    ((range proxy-box-range)
     key
     (function cl-ds.alg.meta:aggregation-function)
     outer-fn
     (arguments list))
  (cl-ds.alg.meta:construct-aggregator
   (read-original-range range)
   (read-key range)
   function
   (proxy-range-aggregator-outer-fn range
                                    key
                                    function
                                    outer-fn
                                    arguments)
   arguments))


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
    (make-proxy range 'forward-proxy-box-range
                :function function
                :key key))
  (:method ((range fundamental-random-access-range) function key)
    (make-proxy range 'random-access-proxy-box-range
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
  (drop-back (read-original-range range) count)
  range)
