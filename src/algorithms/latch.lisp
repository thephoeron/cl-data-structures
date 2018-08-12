(in-package #:cl-ds.alg)


(defclass latch-proxy (proxy-range)
  ((%latches :initarg :latches
             :accessor access-latches)))


(defclass forward-latch-proxy (latch-proxy
                               fundamental-forward-range)
  ())


(defmethod cl-ds:clone ((range latch-proxy))
  (make (type-of range)
        :original-range (cl-ds:clone (read-original-range range))
        :latches (map 'vector #'cl-ds:clone (access-latches range))))


(defmethod cl-ds:traverse (function (range latch-proxy))
  (let ((current (read-original-range range))
        (latches (access-latches range)))
    (iterate
      (for (values value more) = (cl-ds:consume-front current))
      (while more)
      (iterate
        (for latch in-vector latches)
        (for (values open more) = (cl-ds:consume-front latch))
        (unless more
          (return-from cl-ds:traverse range))
        (when (null open)
          (leave range))
        (finally (funcall function value)))
      (finally (return range)))))


(defmethod cl-ds:reset! ((range latch-proxy))
  (cl-ds:reset)
  (setf (access-current range) (cl-ds:clone (read-original-range range))
        (access-latches range) (map 'vector #'cl-ds:clone
                                    (read-original-latches range)))
  (call-next-method))


(defmethod cl-ds:peek-front ((range forward-latch-proxy))
  (let ((current (cl-ds:clone (access-current range)))
        (latches (cl-ds:clone (access-latches range))))
    (iterate
      (for (values value more) = (cl-ds:consume-front current))
      (while more)
      (iterate
        (for latch in-vector latches)
        (for (values open more) = (cl-ds:consume-front latch))
        (unless more
          (return-from cl-ds:peek-front (values nil nil)))
        (when (null open)
          (leave))
        (finally (return-from cl-ds:peek-front (values value t))))
      (finally (return (values nil nil))))))


(defmethod cl-ds:consume-front ((range forward-latch-proxy))
  (let ((current (access-current range))
        (latches (access-latches range)))
    (iterate
      (for (values value more) = (cl-ds:consume-front current))
      (while more)
      (iterate
        (for latch in-vector latches)
        (for (values open more) = (cl-ds:consume-front latch))
        (unless more
          (return-from cl-ds:consume-front (values nil nil)))
        (when (null open)
          (leave))
        (finally (return-from cl-ds:consume-front (values value t))))
      (finally (return (values nil nil))))))


(defclass latch-aggregator (cl-ds.alg.meta:fundamental-aggregator)
  ((%key :initarg :key
         :reader read-key)
   (%outer :initarg :outer
           :reader read-outer)))


(defmethod proxy-range-aggregator-outer-fn ((range latch-proxy)
                                            key
                                            function
                                            outer-fn
                                            arguments)
  (bind (((:slots (%key cl-ds.alg.meta:%key) %function) range)
         (outer-fn (call-next-method)))
    (lambda ()
      (let ((outer (funcall outer-fn)))
        (make 'latch-aggregator
              :key (read-key range)
              :outer outer)))))


(defmethod cl-ds.alg.meta:construct-aggregator ((range latch-proxy)
                                                key
                                                (function cl-ds.alg.meta:aggregation-function)
                                                outer-fn
                                                (arguments list))
  (cl-ds.alg.meta:construct-aggregator
   (read-original-range range)
   (read-key range)
   function
   (proxy-range-aggregator-outer-fn range
                                    key
                                    function
                                    outer-fn
                                    arguments)
   arguments))


(defmethod cl-ds.alg.meta:pass-to-aggregation ((aggregator latch-aggregator)
                                               element)
  (labels ((to-outer (element)
             (if (listp element)
                 (map nil #'to-outer element)
                 (cl-ds.alg.meta:pass-to-aggregation (read-outer aggregator)
                                                     element))))
    (declare (dynamic-extent (function to-outer)))
    (to-outer (funcall (read-key aggregator) element))))


(defmethod cl-ds.alg.meta:aggregator-finished-p ((aggregator latch-aggregator))
  (cl-ds.alg.meta:aggregator-finished-p (read-outer aggregator)))


(defmethod cl-ds.alg.meta:end-aggregation ((aggregator latch-aggregator))
  (cl-ds.alg.meta:end-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:begin-aggregation ((aggregator latch-aggregator))
  (cl-ds.alg.meta:begin-aggregation (read-outer aggregator)))


(defmethod cl-ds.alg.meta:extract-result ((aggregator latch-aggregator))
  (cl-ds.alg.meta:extract-result (read-outer aggregator)))


(defmethod cl-ds.alg.meta:expects-content-p ((aggregator latch-aggregator))
  (cl-ds.alg.meta:expects-content-p (read-outer aggregator)))

