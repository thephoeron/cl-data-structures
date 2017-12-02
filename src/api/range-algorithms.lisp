(in-package #:cl-data-structures)


(defclass proxy-range ()
  ((%original-range :initarg :original-range
                    :reader read-original-range)))


(defclass forward-proxy-range (proxy-range fundamental-forward-range)
  ())


(defclass bidirectional-proxy-range (proxy-range fundamental-bidirectional-range)
  ())


(defclass random-access-proxy-range (proxy-range fundamental-random-access-range)
  ())


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


(defclass forward-group-by-proxy (forward-proxy-range group-by-proxy)
  ())


(defclass bidirectional-group-by-proxy (bidirectional-proxy-range group-by-proxy)
  ())


(defclass random-access-group-by-proxy (random-access-proxy-range group-by-proxy)
  ())


(defgeneric make-proxy (range class
                        &rest all
                        &key &allow-other-keys)
  (:method ((range fundamental-range)
            class &rest all &key &allow-other-keys)
    (apply #'make-instance class :original-range range all))
  (:method ((range group-by-proxy)
            class &rest all &key &allow-other-keys)
    (let ((original-range (read-original-range range))))
    -groups (read-groups range)
      (apply #'make-instance
             (type-of range)
             :original-range (apply #'make-proxy original-range class all)
             :groups (copy-hash-table original-groups)
             :key (read-key range))))


(defclass on-each-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass accumulate-function (aggregation-function)
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


(defgeneric accumulate (function range &key key initial-value)
  (:generic-function-class accumulate-function)
  (:method (function (range fundamental-range) &key (key #'identity) (initial-value nil))
    (apply-aggregation-function range #'accumulate
                                :key key
                                :initial-value initial-value)))


(defclass proxy-box-range ()
  ((%function :initarg :function
              :reader read-function)))


(defclass forward-proxy-box-range (forward-proxy-range proxy-box-range)
  ())


(defclass bidirectional-proxy-box-range (bidirectional-proxy-range proxy-box-range)
  ())


(defclass random-access-proxy-box-range (random-access-proxy-range proxy-box-range)
  ())


(defmethod clone ((range forward-proxy-box-range))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :function (read-function range)))


(defclass hash-table-range (fundamental-random-access-range)
  ((%hash-table :initarg :hash-table
                :reader read-hash-table)
   (%keys :initarg keys
          :reader read-keys)
   (%begin :initarg :begin
           :type fixnum
           :accessor access-begin)
   (%end :initarg :end
         :type fixnum
         :accessor access-end)))


(defmethod clone ((range hash-table-range))
  (make 'hash-table-range
        :keys (read-keys range)
        :begin (access-begin range)
        :end (access-end range)
        :hash-table (read-hash-table range)))


(defmethod consume-front ((range forward-proxy-range))
  (consume-front (read-original-range range)))


(defmethod consume-back ((range bidirectional-proxy-range))
  (consume-back (read-original-range range)))


(defmethod peek-front ((range forward-proxy-range))
  (peek-front (read-original-range range)))


(defmethod peek-back ((range bidirectional-proxy-range))
  (peek-back (read-original-range)))


(defmethod at ((range random-access-proxy-range) location)
  (at (read-original-range range) location))


(defmethod morep ((range forward-proxy-range))
  (morep (read-original-range range)))


(defmethod consume-front ((range forward-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod consume-back ((range bidirectional-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod peek-front ((range forward-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod peek-back ((range bidirectional-proxy-box-range))
  (funcall (read-function range) (call-next-method)))


(defmethod at ((range random-access-proxy-box-range) location)
  (funcall (read-function range) (call-next-method)))


(defmethod consume-front ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys begin)))
          (incf begin)
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod consume-back ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys (decf end))))
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod peek-front ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys begin)))
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod peek-back ((range hash-table-range))
  (with-slots ((begin %begin) (end %end) (ht %hash-table) (keys %keys)) range
    (if (eql begin end)
        (values nil nil)
        (let ((key (aref keys end)))
          (values
           (list* key
                  (gethash key ht))
           t)))))


(defmethod morep ((range hash-table-range))
  (with-slots ((begin %begin) (end %end)) range
    (not (eql begin end))))


(defmethod at ((range hash-table-range) location)
  (with-slots ((ht %hash-table)) range
    (gethash location ht)))


(defun make-hash-table-range (hash-table)
  (let ((keys (coerce (hash-table-keys hash-table) 'vector)))
    (make-instance 'hash-table-range
                   :hash-table hash-table
                   :keys keys
                   :begin 0
                   :end (length keys))))


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
