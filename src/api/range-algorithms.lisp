(in-package #:cl-data-structures)


(defclass proxy-range ()
  ((%original-range :initarg :original-range
                    :reader read-original-range)))


(defclass group-by-proxy (proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%key :initarg key
         :type hash-table
         :reader read-key)))


(defmethod initialize-instance :before ((instance group-by-proxy) &key test key &allow-other-keys)
  (setf (slot-value instance '%groups) (make-hash-table :test test)
        (slot-value instance '%key) key))


(defclass forward-group-by-proxy (group-by-proxy)
  ())


(defclass bidirectional-group-by-proxy (group-by-proxy)
  ())


(defclass random-access-group-by-proxy (group-by-proxy)
  ())


(defgeneric make-proxy (range class
                        &rest all
                        &key &allow-other-keys)
  (:method ((range fundamental-range)
            class &rest all &key &allow-other-keys)
    (apply #'make-instance class :original-range range all))
  (:method ((range group-by-proxy)
            class &rest all &key &allow-other-keys)
    (let ((original-range (read-original-range range))
          (original-groups (read-groups range)))
      (apply #'make-instance
             (type-of range)
             :original-range (apply #'make-proxy original-range class all)
             :groups (copy-hash-table original-groups)
             :key (read-key range)))))


(defclass on-each-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric on-each (function range)
  (:generic-function-class on-each-function)
  (:method (function (range fundamental-range))
    (apply-range-function range #'on-each :function function)))


(defgeneric group-by (range &key test key)
  (:generic-function-class group-by-function)
  (:method ((range fundamental-range) &key (test 'eql) (key #'identity))
    (apply-range-function range #'group-by :test test :key key)))

(defclass forward-proxy-box-range (fundamental-forward-range
                                   proxy-range)
  ((%function :initarg :function
              :reader read-function)))


(defmethod clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :function (read-function range)))


(defmethod clone ((range group-by-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :groups (copy-hash-table (read-groups range))))


(defclass bidirectional-proxy-box-range (forward-proxy-box-range)
  ())


(defclass random-access-proxy-box-range (bidirectional-proxy-box-range)
  ())


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


(defgeneric on-each-proxy-range-from-range (range function)
  (:method :around ((range fundamental-range) function)
    (check-type function (or symbol function))
    (call-next-method))
  (:method ((range fundamental-forward-range) function)
    (make-proxy range 'forward-proxy-box-range
                :function function))
  (:method ((range fundamental-bidirectional-range) function)
    (make-proxy range 'bidirectional-proxy-box-range
                :function function))
  (:method ((range fundamental-random-access-range) function)
    (make-proxy range 'random-access-proxy-box-range
                :function function)))


(defmethod apply-layer ((range fundamental-range)
                        (fn on-each-function)
                        &rest all &key function)
  (declare (ignore all))
  (on-each-proxy-range-from-range range function))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn group-by-function)
                        &rest all &key test key)
  (declare (ignore all))
  (make-proxy range 'forward-group-by-proxy
              :test test
              :key key))


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


(defgeneric change-each! (function range)
  (:generic-function-class transformation!-function))
