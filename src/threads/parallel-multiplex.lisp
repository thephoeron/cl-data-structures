(cl:in-package #:cl-data-structures.threads)


(defclass parallel-forward-multiplex-proxy (cl-ds.alg:forward-multiplex-proxy)
  ((%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-forward-multiplex-proxy))
  '((:maximum-queue-size read-maximum-queue-size)))


(defclass parallel-multiplex-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-multiplex (range &key key function maximum-queue-size)
  (:generic-function-class parallel-multiplex-function)
  (:method (range &key
                    (function #'cl-ds:whole-range)
                    (key #'identity)
                    (maximum-queue-size 512))
    (check-type maximum-queue-size integer)
    (cl-ds:check-argument-bounds maximum-queue-size
                                 (<= 16 maximum-queue-size))
    (ensure-functionf function)
    (ensure-functionf key)
    (cl-ds.alg.meta:apply-range-function
     range #'parallel-multiplex
     (list range :key key :function function :maximum-queue-size maximum-queue-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-multiplex-function)
                                       all)
  (let ((keys (rest all)))
    (make 'parallel-forward-multiplex-proxy
          :original-range range
          :key (getf keys :key)
          :maximum-queue-size (getf keys :maximum-queue-size)
          :function (getf keys :function))))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range parallel-forward-multiplex-proxy)
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
            (aggregate-thread
             (bt:make-thread
              (lambda ()
                (iterate
                  (declare (type fixnum count))
                  (with count = 1)
                  (until (zerop count))
                  (for (op . elt) = (lparallel.queue:pop-queue queue))
                  (switch (op :test eq)
                    (:progress (cl-ds.alg.meta:pass-to-aggregation
                                inner
                                elt))
                    (:start (incf count))
                    (:end (decf count)))))
              :name "Aggregation Thread")))

           ((element)
             (lparallel.queue:push-queue '(:start nil) queue)
             (lparallel:future
               (~>> element (funcall key) (funcall fn)
                    (cl-ds:traverse _
                                    (lambda (element)
                                      (lparallel.queue:push-queue
                                       (cons :progress element) queue))))
               (lparallel.queue:push-queue '(:end nil) queue)))

           ((lparallel.queue:push-queue '(:end nil) queue)
             (handler-case
                 (progn
                   (bt:join-thread aggregate-thread)
                   (setf aggregate-thread nil))
               (t (e) (declare (ignore e))
                 (bt:destroy-thread aggregate-thread)))
             (cl-ds.alg.meta:extract-result inner))))
     function
     arguments)))
