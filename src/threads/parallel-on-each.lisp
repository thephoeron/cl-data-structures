(cl:in-package #:cl-data-structures.threads)


(defclass parallel-on-each-proxy (cl-ds.alg:forward-multiplex-proxy)
  ((%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-on-each-proxy))
  '((:maximum-queue-size read-maximum-queue-size)
    (:chunk-size read-chunk-size)))


(defclass parallel-on-each-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-on-each (range function
                              &key key maximum-queue-size chunk-size)
  (:generic-function-class parallel-on-each-function)
  (:method (range function
            &key (key #'identity) (maximum-queue-size 512) (chunk-size 128))
    (check-type maximum-queue-size integer)
    (check-type chunk-size integer)
    (cl-ds:check-argument-bounds maximum-queue-size (<= 16 maximum-queue-size))
    (cl-ds:check-argument-bounds chunk-size (<= 1 chunk-size))
    (ensure-functionf function)
    (ensure-functionf key)
    (cl-ds.alg.meta:apply-range-function
     range #'parallel-on-each
     (list range function
           :key key
           :maximum-queue-size maximum-queue-size
           :chunk-size chunk-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-on-each-function)
                                       all)
  (let ((keys (cddr all)))
    (make 'parallel-on-each-proxy
          :original-range range
          :key (getf keys :key)
          :maximum-queue-size (getf keys :maximum-queue-size)
          :function (second all))))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range parallel-on-each-proxy)
     outer-constructor
     (function cl-ds.alg.meta:aggregation-function)
     (arguments list))
  (declare (optimize (speed 3) (safety 0)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (or outer-constructor
                       (cl-ds.alg.meta:aggregator-constructor '()
                                                              nil
                                                              function
                                                              arguments)))
         (maximum-queue-size (read-maximum-queue-size range))
         (chunk-size (the fixnum (read-chunk-size range)))
         (fn (ensure-function (cl-ds.alg:read-function range)))
         (key (ensure-function (cl-ds.alg:read-key range))))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq key #'identity))
                         (:variant (eq fn #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn))
            (queue (lparallel.queue:make-queue
                    :fixed-capacity maximum-queue-size))
            (error-lock (bt:make-lock "error lock"))
            (stored-error nil)
            (chunk (make-array chunk-size :fill-pointer 0))
            ((:flet push-chunk ())
             (let ((chunk (copy-array chunk)))
               (declare (type (cl-ds.utils:extendable-vector t) chunk))
               (lparallel.queue:push-queue
                (lparallel:future
                  (assert (array-has-fill-pointer-p chunk))
                  (handler-case (map 'vector fn chunk)
                    (error (e) e)))
                queue))
             (setf (fill-pointer chunk) 0))
            (aggregate-thread
             (bt:make-thread
              (lambda ()
                (iterate
                  (for future = (lparallel.queue:pop-queue queue))
                  (when (null future)
                    (leave))
                  (for elt = (lparallel:future future))
                  (if (vectorp elt)
                      (handler-case
                          (iterate
                            (for e in-vector elt)
                            (cl-ds.alg.meta:pass-to-aggregation inner e))
                        (error (e)
                          (bt:with-lock-held (error-lock)
                            (setf stored-error e))
                          (leave)))
                      (progn
                        (bt:with-lock-held (error-lock)
                          (setf stored-error elt))
                        (leave)))))
              :name "Aggregation Thread")))

           ((element)
             (bt:with-lock-held (error-lock)
               (unless (null stored-error)
                 (error stored-error)))
             (when (= chunk-size (fill-pointer chunk))
               (push-chunk))
             (vector-push-extend element chunk))

           ((unless (zerop (fill-pointer chunk))
              (push-chunk))
             (lparallel.queue:push-queue nil queue)
             (bt:join-thread aggregate-thread)
             (bt:with-lock-held (error-lock)
               (unless (null stored-error)
                 (error stored-error)))
             (cl-ds.alg.meta:extract-result inner))))
     function
     arguments)))
