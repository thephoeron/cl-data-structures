(cl:in-package #:cl-data-structures.threads)


(defclass parallel-group-by-proxy (cl-ds.alg:proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)
   (%key :initarg :key
         :reader read-key)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-group-by-proxy))
  '((:groups read-groups)
    (:chunk-size read-chunk-size)
    (:key read-key)))


(defclass forward-parallel-group-by-proxy (parallel-group-by-proxy
                                           cl-ds:fundamental-forward-range)
  ())


(defclass bidirectional-parallel-group-by-proxy (forward-parallel-group-by-proxy
                                                 bidirectional-proxy-range)
  ())


(defclass random-access-parallel-group-by-proxy (bidirectional-parallel-group-by-proxy
                                                 random-access-proxy-range)
  ())


(defmethod initialize-instance :before ((instance parallel-group-by-proxy)
                                        &key test groups key &allow-other-keys)
  (setf (slot-value instance '%groups) (if (null test)
                                           (copy-hash-table groups)
                                           (make-hash-table :test test))
        (slot-value instance '%key) key))


(defclass parallel-group-by-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))



(defgeneric parallel-group-by (range &key test key groups chunk-size)
  (:generic-function-class parallel-group-by-function)
  (:method (range &key
                    (test 'eql) (key #'identity)
                    (chunk-size 16)
                    (groups (make-hash-table :test test)))
    (cl-ds.alg.meta:apply-range-function range #'parallel-group-by
                                         (list range :test test
                                                     :key key
                                                     :chunk-size chunk-size
                                                     :groups groups))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range parallel-group-by-proxy)
                                                  outer-constructor
                                                  (function cl-ds.alg.meta:aggregation-function)
                                                  (arguments list))
  (bind ((groups-prototype (read-groups range))
         (chunk-size (read-chunk-size range))
         (group-by-key (ensure-function (read-key range)))
         (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq group-by-key #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((groups (copy-hash-table groups-prototype))
            (stored-error nil)
            (error-lock (bt:make-lock)))

           ((element)
             (bt:with-lock-held (error-lock)
               (unless (null stored-error)
                 (error stored-error)))
            (bind ((selected (~>> element (funcall group-by-key)))
                   (group (gethash selected groups)))
              (when (null group)
                (setf group (list (bt:make-lock)
                                  (vect)
                                  (cl-ds.alg.meta:call-constructor outer-fn))
                      (gethash selected groups) group))
              (bind (((lock buffer aggregator) group))
                (vector-push-extend element buffer)
                (unless (< (length buffer) chunk-size)
                  (let ((chunk (copy-array buffer)))
                    (setf (fill-pointer buffer) 0)
                    (lparallel:future
                      (handler-case
                          (iterate
                            (for elt in-vector chunk)
                            (bt:with-lock-held (lock)
                              (cl-ds.alg.meta:pass-to-aggregation aggregator elt)))
                        (error (e)
                          (bt:with-lock-held (error-lock)
                            (setf stored-error e))))))))))

           ((let ((result (copy-hash-table groups-prototype)))
              (maphash (lambda (key group)
                         (bind (((lock buffer aggregator) group))
                           (setf (gethash result key)
                                 (lparallel:future
                                   (iterate
                                     (for c in-vector buffer)
                                     (bt:with-lock-held (lock)
                                       (cl-ds.alg.meta:pass-to-aggregation c aggregator)))
                                   (cl-ds.alg.meta:extract-result aggregator)))))
                       groups)
              (maphash (lambda (key aggregator)
                         (setf (gethash key result)
                               (lparallel:force aggregator)))
                       result)
              (make-instance 'cl-ds.alg:group-by-result-range
                             :hash-table result
                             :keys (~> result hash-table-keys (coerce 'vector))
                             :begin 0
                             :end (hash-table-count result))))

         (iterate
           (for (key group) in-hashtable groups)
           (for (lock buffer aggregator) = group)
           (bt:with-lock-held (lock)
             (cl-ds.alg.meta:cleanup aggregator)))))
     function
     arguments)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'forward-parallel-group-by-proxy
                        :groups (cl-ds.utils:at-list (rest all) :groups)
                        :chunk-size (cl-ds.utils:at-list (rest all) :chunk-size)
                        :key (cl-ds.utils:at-list (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'forward-parallel-group-by-proxy
                        :groups (cl-ds.utils:at-list (rest all) :groups)
                        :chunk-size (cl-ds.utils:at-list (rest all) :chunk-size)
                        :key (cl-ds.utils:at-list (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'bidirectional-parallel-group-by-proxy
                        :groups (cl-ds.utils:at-list (rest all) :groups)
                        :chunk-size (cl-ds.utils:at-list (rest all) :chunk-size)
                        :key (cl-ds.utils:at-list (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'random-access-parallel-group-by-proxy
                        :groups (cl-ds.utils:at-list (rest all) :groups)
                        :chunk-size (cl-ds.utils:at-list (rest all) :chunk-size)
                        :key (cl-ds.utils:at-list (rest all) :key)))
