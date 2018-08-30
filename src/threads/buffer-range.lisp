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
  (make (type-of range)
        :limit (read-limit range)
        :context-function (read-context-function range)
        :original-range (cl-ds.alg:read-original-range range)))


(defun traverse/accross (traverse/accross range function)
  (let* ((queue (lparallel.queue:make-queue :fixed-capacity (read-limit range)))
         (og-range (cl-ds.alg::read-original-range range))
         (context-function (read-context-function range))
         (fn (funcall context-function
                      (lambda ()
                        (funcall traverse/accross
                                 (lambda (data)
                                   (lparallel.queue:push-queue (list* data t)
                                                               queue))
                                 og-range)
                        (lparallel.queue:push-queue '(nil)
                                                    queue))))
         (thread (bt:make-thread fn :name "buffer-range thread"))
         (all-good nil))
    (handler-case
        (iterate
          (for (data . more) = (lparallel.queue:pop-queue queue))
          (while more)
          (funcall function data)
          (finally (bt:join-thread thread)
                   (setf all-good t)))
      (unless all-good
        (bt:destroy-thread thread)))
    range))


(defmethod cl-ds:traverse (function (range buffer-range))
  (traverse/accross #'cl-ds:traverse range function))


(defmethod cl-ds:across (function (range buffer-range))
  (traverse/accross #'cl-ds:across range function))


(defclass thread-buffer-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric thread-buffer (range limit &key context-function)
  (:generic-function-class thread-buffer-function)
  (:method (range limit &key (context-function #'identity))
    (cl-ds.alg.meta:apply-range-function range #'thread-buffer
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
