(cl:in-package #:cl-data-structures.threads)


(defclass buffer-range (cl-ds.alg:proxy-range)
  ((%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range buffer-range))
  '((:maximum-queue-size read-maximum-queue-size)))


(defclass forward-buffer-range (buffer-range cl-ds.alg:forward-proxy-range)
  ())


(defclass bidirectional-buffer-range (buffer-range cl-ds.alg:bidirectional-proxy-range)
  ())


(defclass random-access-buffer-range (buffer-range cl-ds.alg:random-access-proxy-range)
  ())


(defclass thread-buffer-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric thread-buffer (range &key maximum-queue-size)
  (:generic-function-class thread-buffer-function)
  (:method (range &key (maximum-queue-size 512))
    (check-type maximum-queue-size integer)
    (cl-ds:check-argument-bounds maximum-queue-size
                                 (<= 16 maximum-queue-size))
    (cl-ds.alg.meta:apply-range-function
     range #'thread-buffer
     (list range maximum-queue-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn thread-buffer-function)
                                       arguments)
  (cl-ds.alg:make-proxy range 'forward-buffer-range
                        :maximum-queue-size (second arguments)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn thread-buffer-function)
                                       arguments)
  (cl-ds.alg:make-proxy range 'random-access-buffer-range
                        :maximum-queue-size (second arguments)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn thread-buffer-function)
                                       arguments)
  (cl-ds.alg:make-proxy range 'bidirectional-buffer-range
                        :maximum-queue-size (second arguments)))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range buffer-range)
     outer-constructor
     (function cl-ds.alg.meta:aggregation-function)
     (arguments list))
  (declare (optimize (speed 3) (safety 0)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (call-next-method))
         (maximum-queue-size (read-maximum-queue-size range)))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((inner (cl-ds.alg.meta:call-constructor outer-fn))
          (queue (lparallel.queue:make-queue
                  :fixed-capacity maximum-queue-size))
          (error-lock (bt:make-lock "error lock"))
          (stored-error nil)
          ((:flet thread-function ())
           (iterate
             (for (op . elt) = (lparallel.queue:pop-queue queue))
             (until (eq :end op))
             (handler-case (cl-ds.alg.meta:pass-to-aggregation inner elt)
               (error (e) (bt:with-lock-held (error-lock)
                            (setf stored-error e)
                            (leave))))))
          (aggregate-thread (bt:make-thread #'thread-function
                                            :name "Aggregation Thread")))

         ((element)
           (bt:with-lock-held (error-lock)
             (unless (null stored-error)
               (error stored-error)))
           (lparallel.queue:push-queue `(:progress . ,element) queue))

         ((lparallel.queue:push-queue '(:end . nil) queue)
           (bt:join-thread aggregate-thread)
           (bt:with-lock-held (error-lock)
             (unless (null stored-error) (error stored-error)))
           (cl-ds.alg.meta:extract-result inner)))
     function
     arguments)))
