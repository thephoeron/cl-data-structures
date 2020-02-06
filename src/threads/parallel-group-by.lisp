(cl:in-package #:cl-data-structures.threads)


(defclass parallel-group-by-proxy (cl-ds.alg:proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)
   (%maximal-queue-size :initarg :maximal-queue-size
                        :reader read-maximal-queue-size)
   (%key :initarg :key
         :reader read-key)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-group-by-proxy))
  '((:groups read-groups)
    (:maximal-queue-size read-maximal-queue-size)
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



(defgeneric parallel-group-by (range &key test key groups chunk-size maximal-queue-size)
  (:generic-function-class parallel-group-by-function)
  (:method (range &key
                    (test 'eql) (key #'identity)
                    (chunk-size 16)
                    (maximal-queue-size 32)
                    (groups (make-hash-table :test test)))
    (cl-ds.alg.meta:apply-range-function range #'parallel-group-by
                                         (list range :test test
                                                     :key key
                                                     :maximal-queue-size maximal-queue-size
                                                     :chunk-size chunk-size
                                                     :groups groups))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range parallel-group-by-proxy)
                                                  outer-constructor
                                                  (function cl-ds.alg.meta:aggregation-function)
                                                  (arguments list))
  (bind ((groups-prototype (read-groups range))
         (chunk-size (read-chunk-size range))
         (maximal-queue-size (read-maximal-queue-size range))
         (group-by-key (ensure-function (read-key range)))
         (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq group-by-key #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((groups (copy-hash-table groups-prototype))
            (futures (make-array maximal-queue-size
                                 :element-type t
                                 :fill-pointer 0))
            ((:flet scan-futures (&optional force))
             (iterate
               (for fill-pointer = (fill-pointer futures))
               (until (zerop (the fixnum fill-pointer)))
               (for future = (aref futures 0))
               (for fullfilledp = (lparallel:fulfilledp future))
               (for full = (>= fill-pointer maximal-queue-size))
               (when (or force full fullfilledp)
                 (cl-ds.utils:swapop futures 0)
                 (when-let ((e (lparallel:force future)))
                   (error e))
                 (next-iteration))
               (unless force (leave)))))

           ((element)
             (scan-futures)
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
                    (vector-push-extend (lparallel:future
                                          (handler-case
                                              (iterate
                                                (for elt in-vector chunk)
                                                (bt:with-lock-held (lock)
                                                  (cl-ds.alg.meta:pass-to-aggregation
                                                   aggregator elt)))
                                            (error (e) e)))
                     futures))))))

           ((let ((result (copy-hash-table groups-prototype)))
              (scan-futures t)
              (maphash (lambda (key group)
                         (bind (((lock buffer aggregator) group))
                           (setf (gethash result key)
                                 (lparallel:future
                                   (handler-case
                                       (iterate
                                         (for c in-vector buffer)
                                         (bt:with-lock-held (lock)
                                           (cl-ds.alg.meta:pass-to-aggregation c aggregator))
                                         (finally
                                          (bt:with-lock-held (lock)
                                            (return (cons t (cl-ds.alg.meta:extract-result aggregator))))))
                                     (error (e) (cons nil e)))))))
                       groups)
              (maphash (lambda (key aggregator)
                         (bind (((success . result) (lparallel:force aggregator)))
                           (unless success
                             (error result))
                           (setf (gethash key result)
                                 (lparallel:force aggregator))))
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
                        :groups (getf (rest all) :groups)
                        :maximal-queue-size (getf (rest all) :maximal-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'forward-parallel-group-by-proxy
                        :groups (getf (rest all) :groups)
                        :maximal-queue-size (getf (rest all) :maximal-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'bidirectional-parallel-group-by-proxy
                        :groups (getf (rest all) :groups)
                        :maximal-queue-size (getf (rest all) :maximal-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'random-access-parallel-group-by-proxy
                        :groups (getf (rest all) :groups)
                        :maximal-queue-size (getf (rest all) :maximal-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))
