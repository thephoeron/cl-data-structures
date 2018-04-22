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
   (%group-by-key :initarg :group-by-key
                  :reader read-key)))


(defclass linear-group-by-aggregator (group-by-aggregator)
  ((%finished :initform nil
              :reader cl-ds.alg.meta:aggregator-finished-p
              :accessor access-finished)))


(defclass multi-group-by-aggregator (group-by-aggregator
                                     multi-aggregator)
  ((%stages :initarg :stages
            :accessor access-stages)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator group-by-aggregator))
  (iterate
    (for (key value) in-hashtable (read-groups aggregator))
    (begin-aggregation value)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator group-by-aggregator))
  (declare (optimize (debug 3)))
  (iterate
    (for (key value) in-hashtable (read-groups aggregator))
    (end-aggregation value)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator multi-group-by-aggregator))
  (assert (access-stages aggregator))
  (call-next-method)
  (setf (access-stages aggregator) (rest (access-stages aggregator))))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator linear-group-by-aggregator))
  (setf (access-finished aggregator) t)
  (call-next-method))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator multi-group-by-aggregator))
  (~> aggregator access-stages endp))


(defmethod cl-ds.alg.meta:expects-content ((aggregator linear-group-by-aggregator))
  t)


(defmethod cl-ds.alg.meta:expects-content ((aggregator multi-group-by-aggregator))
  (cl-ds.alg.meta:expects-content-with-stage (~> aggregator access-stages first)
                                             aggregator))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator group-by-aggregator)
                                               element)
  (bind (((:slots %group-by-key %groups %outer-fn) aggregator)
         (selected (~>> element (funcall %group-by-key)))
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
     (if (typep function 'cl-ds.alg.meta:multi-aggregation-function)
         (lambda ()
           (make 'multi-group-by-aggregator
                 :groups groups
                 :outer-fn outer-fn
                 :stages (apply #'cl-ds.alg.meta:multi-aggregation-stages
                                function
                                arguments)
                 :group-by-key (read-key range)
                 :key key))
         (lambda ()
           (make 'linear-group-by-aggregator
                 :groups groups
                 :outer-fn outer-fn
                 :key key
                 :group-by-key (read-key range)))))
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
