(in-package #:cl-data-structures.threads)


(defclass in-parallel-range (cl-ds.alg:transparent-to-chunking-mixin
                             cl-ds.alg:proxy-range)
  ((%limit :initarg :limit
           :reader read-limit)
   (%context-function :initarg :context-function
                      :reader read-context-function)))


(defclass forward-in-parallel-range (in-parallel-range
                                     cl-ds.alg:forward-proxy-range)
  ())


(defclass bidirectional-in-parallel-range (in-parallel-range
                                           cl-ds.alg:bidirectional-proxy-range)
  ())


(defclass random-in-parallel-range (in-parallel-range
                                    cl-ds.alg:random-access-proxy-range)
  ())


(defmethod cl-ds:clone ((range in-parallel-range))
  (make (type-of range)
        :limit (read-limit range)
        :context-function (read-context-function range)
        :original-range (cl-ds.alg:read-original-range range)))


(defun make-in-parallel-read-thread (chunked-range limit context-function)
  (bind ((queue (lparallel.queue:make-queue :fixed-capacity limit))
         ((:flet push-queue (element))
          (lparallel.queue:push-queue element queue))
         ((:flet to-vector (x))
          (lret ((vector (make-array 128 :adjustable t :fill-pointer 0)))
            (cl-ds:traverse (rcurry #'vector-push-extend vector) x)))
         ((:flet impl ())
          (funcall context-function
                   (lambda ()
                     (cl-ds:traverse
                      (lambda (x)
                        (handler-case (~> (to-vector x)
                                          (list* t)
                                          lparallel:future
                                          push-queue)
                          (error (e) (push-queue (list* e :error)))))
                      chunked-range)))
          (push-queue (list* nil nil)))
         (thread (bt:make-thread #'impl :name "in-parallel dispatching thread")))
    (list* thread queue)))


(defun traverse/accross-thread-buffer-range (traverse/accross range function)
  (bind ((og-range (cl-ds.alg::read-original-range range))
         (chunked-range (cl-ds:chunked og-range))
         (pushing nil))
    (if (null chunked-range)
        (funcall traverse/accross function og-range)
        (unwind-protect
             (progn
               (lparallel:check-kernel)
               (iterate
                 (with (thread . queue) =
                       (make-in-parallel-read-thread chunked-range
                                                     (read-limit range)
                                                     (read-context-function range)))
                 (with p = (setf pushing thread))
                 (for future = (lparallel.queue:pop-queue queue))
                 (for (data . more) = (lparallel:force future))
                 (while more)
                 (if (eq :error more)
                     (error data)
                     (map nil function data))
                 (finally (bt:join-thread p)
                          (setf pushing nil))))
          (unless (null pushing)
            (bt:destroy-thread pushing))))
    range))


(defmethod cl-ds:traverse (function (range in-parallel-range))
  (traverse/accross-thread-buffer-range #'cl-ds:traverse range function))


(defmethod cl-ds:across (function (range in-parallel-range))
  (traverse/accross-thread-buffer-range #'cl-ds:across range function))


(defclass in-parallel-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric in-parallel (range &key limit context-function)
  (:generic-function-class in-parallel-function)
  (:method (range &key (limit 512) (context-function #'funcall))
    (cl-ds.alg.meta:apply-range-function range #'in-parallel
                                         :limit limit
                                         :context-function context-function)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn in-parallel-function)
                                       &rest all &key limit context-function)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'forward-in-parallel-range
                        :limit limit
                        :context-function context-function))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn in-parallel-function)
                                       &rest all &key limit context-function)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'random-in-parallel-range
                        :limit limit
                        :context-function context-function))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn in-parallel-function)
                                       &rest all &key limit context-function)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'bidirectional-in-parallel-range
                        :limit limit
                        :context-function context-function))
