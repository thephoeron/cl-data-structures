(in-package #:cl-data-structures.algorithms)


(defclass group-by-proxy (proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%key :initarg :key
         :reader read-key)))


(defclass forward-group-by-proxy (group-by-proxy fundamental-forward-range)
  ())


(defclass bidirectional-group-by-proxy (forward-group-by-proxy fundamental-bidirectional-range)
  ())


(defclass random-access-group-by-proxy (bidirectional-group-by-proxy fundamental-random-access-range)
  ())


(defmethod clone ((range group-by-proxy))
  (make-instance (type-of range)
                 :original-range (clone (read-original-range range))
                 :groups (copy-hash-table (read-groups range))
                 :key (read-key range)))


(defmethod initialize-instance :before ((instance group-by-proxy)
                                        &key test key &allow-other-keys)
  (setf (slot-value instance '%groups) (make-hash-table :test test)
        (slot-value instance '%key) key))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass group-by-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%outer-fn :initarg :outer-fn
              :reader read-outer-fn)
   (%key :initarg :key
         :reader read-key)))


(defclass linear-group-by-aggregator (group-by-aggregator)
  ((%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator group-by-aggregator))
  nil)


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator group-by-aggregator))
  (iterate
    (for (key value) in-hashtable (read-groups aggregator))
    (end-aggregation value)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator linear-group-by-aggregator))
  (setf (access-finished aggregator) t)
  (call-next-method))


(defmethod cl-ds.alg.meta:expects-content ((aggregator linear-group-by-aggregator))
  t)


(defclass multi-group-by-aggregator (group-by-aggregator)
  ())


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator group-by-aggregator)
                                               element)
  (bind (((:slots %key %groups %outer-fn) aggregator)
         (selected (~>> element (funcall %key)))
         (group (gethash selected %groups)))
    (when (null group)
      (setf group (funcall %outer-fn)
            (gethash selected %groups) group)
      (cl-ds.alg.meta:begin-aggregation group))
    (cl-ds.alg.meta:pass-to-aggregation group element)))


(defmethod cl-ds.alg.meta:extract-result ((aggregator group-by-aggregator))
  (bind (((:slots %key %groups %outer-fn) aggregator)
         (groups (copy-hash-table %groups)))
    (maphash (lambda (key aggregator)
               (setf (gethash key groups) (cl-ds.alg.meta:extract-result aggregator)))
             %groups)
    (make-hash-table-range groups)))


(defgeneric group-by (range &key test key)
  (:generic-function-class group-by-function)
  (:method (range &key (test 'eql) (key #'identity))
    (apply-range-function range #'group-by :test test :key key)))


(defmethod cl-ds.alg.meta:construct-aggregator ((range group-by-proxy)
                                                key
                                                (function cl-ds.alg.meta:aggregation-function)
                                                outer-fn
                                                (arguments list))
  (cl-ds.alg.meta:construct-aggregator
   (read-original-range range)
   key
   function
   (bind (((:slots %groups) range)
          (groups (copy-hash-table %groups))
          (outer-fn (or outer-fn (lambda () (call-next-method)))))
     (lambda ()
       (make 'linear-group-by-aggregator
             :groups groups
             :outer-fn outer-fn
             :key (read-key range))))
   arguments))


(defmethod make-proxy ((range group-by-proxy)
                       class &rest all &key &allow-other-keys)
  (let ((original-range (read-original-range range))
        (original-groups (read-groups range)))
    (apply #'make-instance
           (type-of range)
           :original-range (apply #'make-proxy original-range class all)
           :groups (copy-hash-table original-groups)
           :key (read-key range))))


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
