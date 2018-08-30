(in-package #:cl-data-structures.threads)


(defclass buffer-range (cl-ds.alg:proxy-range)
  ((%limit :initarg :limit
           :reader read-limit)
   (%context-function :initarg :context-function
                      :reader read-context-function)))


(defclass forward-buffer-range (buffer-range cl-ds.alg::forward-proxy-range)
  ())


(defclass bidirectional-buffer-range (buffer-range cl-ds.alg::bidirectional-proxy-range)
  ())


(defclass random-access-buffer-range (buffer-range cl-ds.alg::random-access-proxy-range)
  ())


(defmethod cl-ds:clone ((range buffer-range))
  (lret ((result (call-next-method range)))
    (setf (slot-value result '%limit) (read-limit range)
          (slot-value result '%context-function) (read-context-function range))))


(defmethod cl-ds:traverse (function (range buffer-range))
  (let* ((queue (lparallel.queue:make-queue :fixed-capacity (read-limit range)))
         (orginal-range (cl-ds.alg::read-original-range range))
         (context-function (read-context-function range))
         (function (funcall context-function
                            (lambda ()
                              (iterate
                                (for (values data more) =
                                     (cl-ds:consume-front original-range))
                                (while more)
                                (lparallel.queue:push-queue (list* data more)
                                                            queue)
                                (finally (lparallel.queue:push-queue '(nil)
                                                                     queue))))))
         (thread (bt:make-thread function)))
    (iterate
      (for (data . more) = (lparallel.queue:pop-queue queue))
      (while more)
      (funcall function data))
    range))


(defclass thread-buffer-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric thread-buffer (range limit &key context-function)
  (:generic-function-class thread-buffer-function)
  (:method (range limit &key (context-function #'identity))
    (apply-range-function range #'thread-buffer
                          :limit limit
                          :context-function context-function)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn thread-buffer-function)
                                       &rest all &key limit context-function)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'forward-buffer-range
                        :limit limit
                        :context-function context-function))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn thread-buffer-function)
                                       &rest all &key limit context-function)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'random-access-buffer-range
                        :limit limit
                        :context-function context-function))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn thread-buffer-function)
                                       &rest all &key limit context-function)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'bidirectional-buffer-range
                        :limit limit
                        :context-function context-function))
