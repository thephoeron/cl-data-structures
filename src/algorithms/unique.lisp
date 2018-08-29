(in-package #:cl-data-structures.algorithms)


(defclass distinct-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%seen :initarg :seen
          :reader read-seen)))


(defclass forward-distinct-proxy (distinct-proxy
                                  fundamental-forward-range)
  ())


(defmethod cl-ds:clone ((range distinct-proxy))
  (make (type-of range)
        :key (read-key range)
        :seen (copy-hash-table (read-seen range))
        :original-range (read-original-range range)))


(defmethod cl-ds:traverse (function (range distinct-proxy))
  cl-ds.utils:todo
  range)


(defmethod cl-ds:across (function (range distinct-proxy))
  cl-ds.utils:todo
  range)


(defmethod cl-ds:reset! ((range distinct-proxy))
  (clrhash (read-seen range))
  (call-next-method))


(defmethod cl-ds:peek-front ((range forward-distinct-proxy))
  cl-ds.utils:todo)


(defmethod cl-ds:consume-front ((range forward-distinct-proxy))
  cl-ds.utils:todo)


(defclass distinct-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric distinct (range &key key test)
  (:generic-function-class distinct-function)
  (:method (range &key (key #'identity) (test 'eql))
    (ensure-functionf key)
    (apply-range-function range #'distinct :key key :test test)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function distinct-function)
                        &rest all &key key test)
  (declare (ignore all))
  (make 'forward-distinct-proxy
        :key key
        :seen (make-hash-table :test test)
        :original-range range))
