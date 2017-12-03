(in-package #:cl-data-structures)


(defclass group-by-proxy ()
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%key :initarg key
         :type hash-table
         :reader read-key)))


(defmethod clone ((range group-by-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :groups (copy-hash-table (read-groups range))))

(defmethod initialize-instance :before ((instance group-by-proxy) &key test key &allow-other-keys)
  (setf (slot-value instance '%groups) (make-hash-table :test test)
        (slot-value instance '%key) key))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric group-by (range &key test key)
  (:generic-function-class group-by-function)
  (:method ((range fundamental-range) &key (test 'eql) (key #'identity))
    (apply-range-function range #'group-by :test test :key key)))


(defmethod apply-aggregation-function ((range group-by-proxy)
                                       (function aggregation-function)
                                       &rest all &key &allow-other-keys)
  (let ((clone (clone (read-original-range range)))
        (groups (copy-hash-table (read-groups range)))
        (key (read-key range)))
    (iterate
      (while (morep clone))
      (for elt = (consume-front clone))
      (for k = (funcall key elt))
      (aggregate
       function
       (ensure (gethash k groups)
         (apply #'make-state function all))
       elt))
    (let ((result-table (copy-hash-table (read-groups range)
                                         :size (hash-table-size groups))))
      (maphash (lambda (key state)
                 (setf (gethash key result-table) (state-result function state)))
               groups)
      (make-hash-table-range result-table))))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'forward-group-by-proxy
              :test test
              :key key))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'bidirectional-group-by-proxy
              :test test
              :key key))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'random-access-group-by-proxy
              :test test
              :key key))
