(in-package #:cl-data-structures.utils)


(defclass pipe-fragment ()
  ((%thread :reader read-thread
            :initform nil)
   (%sinks :initform (vect)
           :reader read-sinks)
   (%sinks-lock :initform (bt:make-lock)
                :reader read-sinks-lock)
   (%operation :initarg :operation
               :reader read-operation)
   (%queue :reader read-queue))
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric add-into-queue (pipe x)
  (:method ((obj pipe-fragment) x)
    (lparallel.queue:push-queue (read-queue obj) (cons nil x))))


(flet ((make-finalizer (queue)
         (lambda () (lparallel.queue:push-queue queue '(t . nil))))
       (make-worker (obj)
         (bind (((:slots %queue %operation %sinks-lock %sinks) obj))
           (lambda () (iterate
                   (for (end . content) = (lparallel.queue:pop-queue %queue))
                   (until end)
                   (setf content (funcall %operation content))
                   (bt:with-lock-held (%sinks-lock)
                     (map nil (rcurry #'funcall content) %sinks)))))))
  (defmethod initialize-instance :after ((obj pipe-fragment)
                                         &key
                                           (queue-size nil queue-size-bound)
                                         &allow-other-keys)
    (bind ((queue (if queue-size-bound
                      (lparallel.queue:make-queue :fixed-capacity queue-size)
                      (lparallel.queue:make-queue)))
           ((:slots %queue %thread %sinks %operation %sinks-lock) obj))
      (setf %queue queue)
      (c2mop:set-funcallable-instance-function obj (curry #'add-into-queue obj))
      (trivial-garbage:finalize obj (make-finalizer %queue))
      (setf %thread (bt:make-thread (make-worker obj))))))


(defgeneric add-sinks (pipe sink &rest sinks)
  (:method ((pipe pipe-fragment) sink &rest sinks)
    (let ((sinks (cons sink sinks)))
      (bt:with-lock-held ((read-sinks-lock pipe))
        (iterate
          (for sink in sinks)
          (vector-push-extend sink (read-queue pipe)))))
    pipe))


(defgeneric end-execution (pipe)
  (:method ((pipe pipe-fragment))
    (bind (((:slots %thread %queue) pipe))
      (lparallel.queue:push-queue (cons t nil) %queue)
      (bt:join-thread %thread))
    pipe))


(defun make-pipe-fragment (operation &key (queue-size nil queue-size-bound) sinks)
  (let ((result (if queue-size-bound
                    (make 'pipe-fragment
                          :operation operation
                          :queue-size queue-size)
                    (make 'pipe-fragment :operation operation))))
    (apply #'add-sinks result sinks)
    result))
