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
  (bind ((og-dictionary (~> range access-seen-elements))
         (dictionary (if can-mutate
                         og-dictionary
                         (cl-ds:become-transactional og-dictionary))))
    (cl-ds:mod-bind (item found value) (cl-ds:add! dictionary element t)
      (not found))))


(defmethod cl-ds:clone ((range unique-proxy))
  (let ((dictionary (access-seen-elements range)))
    (setf (access-seen-elements range)
          (cl-ds:become-transactional dictionary))
    (make (type-of range)
          :key (read-key range)
          :seen-elements (cl-ds:become-transactional dictionary))))


(defclass unique-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric unique (range &key key hash-function test)
  (:generic-function-class unique-function)
  (:method (range &key
                    (key #'identity)
                    (hash-function #'sxhash)
                    (test #'eql))
    (apply-range-function range #'unique
                          :key key
                          :hash-function hash-function
                          :test test)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function unique-function)
                        &rest all &key key hash-function test)
  (declare (ignore all))
  (make 'bidirectional-filtering-proxy
        :seen-elements (cl-ds.dicts.hamt:make-transactional-hamt-dictionary
                        hash-function test)
        :key key))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function unique-function)
                        &rest all &key key hash-function test)
  (declare (ignore all))
  (make 'forward-filtering-proxy
        :seen-elements (cl-ds.dicts.hamt:make-transactional-hamt-dictionary
                        hash-function test)
        :key key))
