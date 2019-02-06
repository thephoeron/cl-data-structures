(in-package #:cl-data-structures.algorithms)


(defclass unique-proxy (filtering-proxy)
  ((%seen-elements :initarg :seen-elements
                   :accessor access-seen-elements)))


(defclass forward-unique-proxy (unique-proxy
                                forward-filtering-proxy)
  ())


(defclass bidirectional-unique-proxy (unique-proxy
                                      bidirectional-filtering-proxy)
  ())


(defmethod should-skip ((range unique-proxy) element can-mutate)
  (let ((dictionary (~> range access-seen-elements)))
    (prog1 (gethash element dictionary)
      (when can-mutate
        (setf (gethash element dictionary) t)))))


(defmethod cl-ds:clone ((range unique-proxy))
  (~>> range
       access-seen-elements
       copy-hash-table
       (make (type-of range)
             :key (read-key range)
             :seen-elements _)))


(defclass unique-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric unique (range &key key test)
  (:generic-function-class unique-function)
  (:method (range &key
                    (key #'identity)
                    (test 'eql))
    (apply-range-function range #'unique
                          :key key
                          :test test)))


(defmethod cl-ds:reset! ((range unique-proxy))
  (~> range access-seen-elements clrhash)
  (call-next-method))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function unique-function)
                        &rest all &key key test)
  (declare (ignore all))
  (make 'bidirectional-filtering-proxy
        :seen-elements (make-hash-table :test test)
        :key key))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function unique-function)
                        &rest all &key key test)
  (declare (ignore all))
  (make 'forward-filtering-proxy
        :seen-elements (make-hash-table :test test)
        :key key))
