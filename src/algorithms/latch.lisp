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
      (for go = t)
      (iterate
        (for latch in-vector latches)
        (for (values open more) = (cl-ds:consume-front latch))
        (unless more
          (return-from cl-ds:traverse range))
        (when (null open)
          (setf go nil))
        (finally (when go (funcall function value))))
      (finally (return range)))))


(defmethod cl-ds:reset! ((range latch-proxy))
  (cl-ds:reset! (access-latches range))
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
