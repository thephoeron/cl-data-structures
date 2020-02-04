(cl:in-package #:cl-data-structures.threads)


(defclass parallel-on-each-proxy (cl-ds.alg:forward-proxy-box-range)
  ((%maximal-queue-size :initarg :maximal-queue-size
                        :reader read-maximal-queue-size)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-on-each-proxy))
  '((:maximal-queue-size read-maximal-queue-size)
    (:chunk-size read-chunk-size)))


(defclass parallel-on-each-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-on-each (range function
                              &key key maximal-queue-size chunk-size)
  (:generic-function-class parallel-on-each-function)
  (:method (range function
            &key (key #'identity) (maximal-queue-size 512) (chunk-size 128))
    (check-type maximal-queue-size integer)
    (check-type chunk-size integer)
    (cl-ds:check-argument-bounds maximal-queue-size
                                 (<= 16 maximal-queue-size))
    (cl-ds:check-argument-bounds chunk-size (<= 1 chunk-size))
    (ensure-functionf function)
    (ensure-functionf key)
    (cl-ds.alg.meta:apply-range-function
     range #'parallel-on-each
     (list range function
           :key key
           :maximal-queue-size maximal-queue-size
           :chunk-size chunk-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-on-each-function)
                                       all)
  (let ((keys (cddr all)))
    (make-instance 'parallel-on-each-proxy
                   :original-range range
                   :chunk-size (getf keys :chunk-size)
                   :key (getf keys :key)
                   :maximal-queue-size (getf keys :maximal-queue-size)
                   :function (second all))))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range parallel-on-each-proxy)
     outer-constructor
     (function cl-ds.alg.meta:aggregation-function)
     (arguments list))
  (declare (optimize (speed 3) (safety 0)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (or outer-constructor
                       (cl-ds.alg.meta:aggregator-constructor
                        '() nil function arguments)))
         (maximal-queue-size (read-maximal-queue-size range))
         (chunk-size (the fixnum (read-chunk-size range)))
         (fn (ensure-function (cl-ds.alg:read-function range)))
         (key (ensure-function (cl-ds.alg:read-key range))))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq key #'identity))
                         (:variant (eq fn #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn))
            (queue (lparallel:make-channel
                    :fixed-capacity maximal-queue-size))
            (error-lock (bt:make-lock "error lock"))
            (stored-error nil)
            (chunk (make-array chunk-size :fill-pointer 0))
            ((:flet push-chunk ())
             (let ((chunk (copy-array chunk)))
               (declare (type (cl-ds.utils:extendable-vector t) chunk))
               (lparallel:submit-task
                queue
                (lambda ()
                  (assert (array-has-fill-pointer-p chunk))
                  (handler-case (cl-ds.utils:transform fn chunk)
                    (error (e) e)))))
             (setf (fill-pointer chunk) 0))
            ((:flet thread-function ())
             (iterate
               (for elt = (lparallel:receive-result queue))
               (when (null elt)
                 (leave))
               (if (vectorp elt)
                   (handler-case
                       (iterate
                         (with length = (length elt))
                         (for i from 0 below length)
                         (for e = (aref elt i))
                         (cl-ds.alg.meta:pass-to-aggregation inner e))
                     (error (e)
                       (bt:with-lock-held (error-lock)
                         (setf stored-error e))
                       (leave)))
                   (progn
                     (bt:with-lock-held (error-lock)
                       (setf stored-error elt))
                     (leave)))))
            (aggregate-thread (bt:make-thread #'thread-function
                                              :name "Aggregation Thread")))

           ((element)
             (bt:with-lock-held (error-lock)
               (unless (null stored-error)
                 (error stored-error)))
            (unless (< (fill-pointer chunk) chunk-size)
               (push-chunk))
             (vector-push-extend element chunk))

           ((bt:with-lock-held (error-lock)
              (unless (null stored-error)
                (error stored-error)))
             (unless (zerop (fill-pointer chunk))
              (push-chunk))
             (lparallel:submit-task queue (constantly nil))
             (bt:join-thread aggregate-thread)
             (setf aggregate-thread nil)
             (bt:with-lock-held (error-lock)
               (unless (null stored-error)
                 (error stored-error)))
             (cl-ds.alg.meta:extract-result inner))

         (when aggregate-thread
           (bt:destroy-thread aggregate-thread))
         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
