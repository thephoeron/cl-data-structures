(cl:in-package #:cl-data-structures.threads)


(defclass parallel-forward-multiplex-proxy (cl-ds.alg:forward-multiplex-proxy)
  ((%maximal-queue-size :initarg :maximal-queue-size
                        :reader read-maximal-queue-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-forward-multiplex-proxy))
  '((:maximal-queue-size read-maximal-queue-size)))


(defclass parallel-multiplex-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-multiplex (range &key key function maximal-queue-size)
  (:generic-function-class parallel-multiplex-function)
  (:method (range &key
                    (function #'cl-ds:whole-range)
                    (key #'identity)
                    (maximal-queue-size 512))
    (check-type maximal-queue-size integer)
    (cl-ds:check-argument-bounds maximal-queue-size
                                 (<= 16 maximal-queue-size))
    (ensure-functionf function)
    (ensure-functionf key)
    (cl-ds.alg.meta:apply-range-function
     range #'parallel-multiplex
     (list range :key key :function function
                 :maximal-queue-size maximal-queue-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-multiplex-function)
                                       all)
  (let ((keys (rest all)))
    (make 'parallel-forward-multiplex-proxy
          :original-range range
          :key (getf keys :key)
          :maximal-queue-size (getf keys :maximal-queue-size)
          :function (getf keys :function))))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range parallel-forward-multiplex-proxy)
     outer-constructor
     (function cl-ds.alg.meta:aggregation-function)
     (arguments list))
  (declare (optimize (speed 3) (safety 0)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (or outer-constructor
                       (cl-ds.alg.meta:aggregator-constructor
                        '() nil function arguments)))
         (maximal-queue-size (read-maximal-queue-size range))
         (fn (ensure-function (cl-ds.alg:read-function range)))
         (key (ensure-function (cl-ds.alg:read-key range)))
         (futures-lock (bt:make-lock "futures lock"))
         (queue (lparallel.queue:make-queue
                 :fixed-capacity maximal-queue-size))
         (futures (make-array maximal-queue-size
                              :element-type t
                              :fill-pointer 0))
         ((:flet scan-futures (&optional force))
          (bt:with-lock-held (futures-lock)
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
         ((:flet thread-function ())
          (iterate
            (declare (type fixnum count))
            (with count = 0)
            (for (op elt inner) = (lparallel.queue:pop-queue queue))
            (switch (op :test eq)
              (:progress (handler-case (cl-ds.alg.meta:pass-to-aggregation
                                        inner
                                        elt)
                           (error (e)
                             (bt:with-lock-held (futures-lock)
                               (setf (fill-pointer futures) 0)
                               (vector-push-extend e futures))
                             (leave))))
              (:start (incf count))
              (:end (decf count)))
            (until (zerop count))))
         (aggregation-thread-lock (bt:make-lock))
         (aggregate-thread nil))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq key #'identity))
                         (:variant (eq fn #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn)))

           ((element)
             (bt:with-lock-held (aggregation-thread-lock)
               (when (null aggregate-thread)
                 (setf aggregate-thread
                       (bt:make-thread #'thread-function
                                       :name "Aggregation Thread"))))
             (scan-futures)
             (lparallel.queue:push-queue '(:start nil inner) queue)
             (vector-push-extend
              (lparallel:future
                (handler-case
                    (progn
                      (~>> element (funcall key) (funcall fn)
                           (cl-ds:traverse _
                                           (lambda (element)
                                             (lparallel.queue:push-queue
                                              (list :progress element inner) queue))))
                      (lparallel.queue:push-queue '(:end nil inner) queue)
                      nil)
                  (error (e) e)))
              futures))

           ((lparallel.queue:push-queue '(:end nil inner) queue)
             (scan-futures t)
             (bt:join-thread aggregate-thread)
             (setf aggregate-thread nil)
             (cl-ds.alg.meta:extract-result inner))

         (ignore-errors (bt:destroy-thread aggregate-thread))
         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
