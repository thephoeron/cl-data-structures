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
   (%end-on-empty :initarg :end-on-empty
                  :reader read-end-on-empty)
   (%queue :reader read-queue))
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric add-into-queue (pipe x)
  (:method ((obj pipe-fragment) x)
    (lparallel.queue:push-queue (cons nil x) (read-queue obj))))


(flet ((make-finalizer (queue)
         (lambda () (lparallel.queue:push-queue '(t . nil) queue))))
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
      (trivial-garbage:finalize obj (make-finalizer %queue)))))


(defgeneric add-sinks (pipe sink &rest sinks)
  (:method ((pipe pipe-fragment) sink &rest sinks)
    (let ((sinks (cons sink sinks)))
      (bt:with-lock-held ((read-sinks-lock pipe))
        (iterate
          (for sink in sinks)
          (vector-push-extend sink (read-sinks pipe)))))
    pipe))


(defgeneric end-execution (pipe)
  (:method ((pipe pipe-fragment))
    (bind (((:slots %thread %queue) pipe))
      (lparallel.queue:push-queue (cons t nil) %queue)
      (when (slot-boundp pipe '%thread)
        (bt:join-thread %thread)))
    pipe))


(flet ((make-worker (obj)
         (bind (((:slots %queue %operation %sinks-lock %sinks) obj))
           (if (read-end-on-empty obj)
               (lambda ()
                 (iterate
                   (until (lparallel.queue:queue-empty-p %queue))
                   (for (end . content) = (lparallel.queue:pop-queue %queue))
                   (unless end
                     (setf content (funcall %operation content obj))
                     (bt:with-lock-held (%sinks-lock)
                       (map nil (rcurry #'funcall content) %sinks)))))
               (lambda ()
                 (iterate
                   (for (end . content) = (lparallel.queue:pop-queue %queue))
                   (until end)
                   (setf content (funcall %operation content))
                   (bt:with-lock-held (%sinks-lock)
                     (map nil (rcurry #'funcall content) %sinks))))))))
    (defgeneric start-execution (pipe)
      (:method ((pipe pipe-fragment))
        (setf (slot-value pipe '%thread)
              (bt:make-thread (make-worker pipe))))))


(defun make-pipe-fragment (operation
                           &key
                             (queue-size nil queue-size-bound)
                             sinks
                             end-on-empty)
  (let ((result (if queue-size-bound
                    (make 'pipe-fragment
                          :operation operation
                          :queue-size queue-size
                          :end-on-empty end-on-empty)
                    (make 'pipe-fragment
                          :operation operation
                          :end-on-empty end-on-empty))))
    (unless (endp sinks)
      (apply #'add-sinks result sinks))
    result))


(defclass future-carousel ()
  ((%vector :initarg :vector
            :type vector
            :reader read-vector)
   (%index :initform 0
           :type fixnum
           :accessor access-index)
   (%function :initarg :function
              :reader read-function))
  (:metaclass closer-mop:funcallable-standard-class))


(defun make-future-carousel (function &optional count)
  (when (null count)
    (lparallel.kernel:check-kernel)
    (setf count (* 2 (lparallel.kernel:kernel-worker-count))))
  (check-type count fixnum)
  (assert (> count 0))
  (make 'future-carousel
        :vector (make-array count :initial-element nil)
        :function function))


(defmethod end-execution ((obj future-carousel))
  (map nil #'bt:join-thread (read-vector obj)))


(defmethod initialize-instance ((obj future-carousel) &key &allow-other-keys)
  (call-next-method)
  (c2mop:set-funcallable-instance-function
   obj
   (lambda (&rest all)
     (bind (((:slots %vector %index %function) obj))
       (setf %index (mod (1+ %index) (array-dimension %vector 0)))
       (when (bt:threadp (aref %vector %index))
         (bt:join-thread (aref %vector %index)))
       (setf (aref %vector %index) (bt:make-thread (curry #'apply %function all)))))))
