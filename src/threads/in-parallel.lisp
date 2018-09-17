(in-package #:cl-data-structures.threads)


(defclass in-parallel-range (cl-ds.alg:transparent-to-chunking-mixin
                             cl-ds.alg:proxy-range)
  ((%limit :initarg :limit
           :reader read-limit)))


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
        :original-range (cl-ds.alg:read-original-range range)))


(defun traverse/accross-thread-buffer-range (traverse/accross range function)
  (bind ((og-range (cl-ds.alg::read-original-range range))
         (chunked-range (cl-ds:chunked og-range))
         (pushing nil))
    (if (null chunked-range)
        (funcall traverse/accross og-range function)
        (unwind-protect
             (iterate
               (with queue = (lparallel.queue:make-queue
                              :fixed-capacity (read-limit range)))
               (with p =
                     (setf pushing
                           (bt:make-thread
                            (lambda ()
                              (cl-ds:traverse
                               (lambda (x &aux (vector (make-array 128
                                                              :adjustable t
                                                              :fill-pointer 0)))
                                 (handler-case
                                     (progn
                                       (cl-ds:traverse (rcurry #'vector-push-extend vector) x)
                                       (lparallel.queue:push-queue (list* vector t) queue))
                                   (error (e) (lparallel.queue:push-queue (list* e :error) queue))))
                               chunked-range)
                              (lparallel.queue:push-queue (list* nil nil) queue)))))
               (for (data . more) = (lparallel.queue:pop-queue queue))
               (while more)
               (if (eq :error more)
                   (signal data)
                   (map nil function data))
               (finally (bt:join-thread p)))
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


(defgeneric in-parallel (range &key limit)
  (:generic-function-class in-parallel-function)
  (:method (range &key (limit 512))
    (cl-ds.alg.meta:apply-range-function range #'in-parallel :limit 512)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn in-parallel-function)
                                       &rest all &key limit)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'forward-in-parallel-range :limit limit))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn in-parallel-function)
                                       &rest all &key)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'random-in-parallel-range))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn in-parallel-function)
                                       &rest all &key)
  (declare (ignore all))
  (cl-ds.alg:make-proxy range 'bidirectional-in-parallel-range))
