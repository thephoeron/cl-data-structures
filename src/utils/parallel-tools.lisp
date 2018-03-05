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
           (lambda () (iterate
                   (for (end . content) = (lparallel.queue:pop-queue %queue))
                   (until end)
                   (setf content (funcall %operation content))
                   (bt:with-lock-held (%sinks-lock)
                     (map nil (rcurry #'funcall content) %sinks)))))))
    (defgeneric start-execution (pipe)
      (:method ((pipe pipe-fragment))
        (setf (slot-value pipe '%thread) (bt:make-thread (make-worker pipe))))))


(defun make-pipe-fragment (operation &key (queue-size nil queue-size-bound) sinks)
  (let ((result (if queue-size-bound
                    (make 'pipe-fragment
                          :operation operation
                          :queue-size queue-size)
                    (make 'pipe-fragment :operation operation))))
    (unless (endp sinks)
      (apply #'add-sinks result sinks))
    result))


(defclass future-carousel ()
  ((%vector :initarg :vector
            :type vector
            :reader read-vector)
   (%index :initform 0
           :type fixnum
           :accessor access-index))
  (:metaclass closer-mop:funcallable-standard-class))


(defun make-future-carousel (count)
  (check-type count fixnum)
  (assert (> count 0))
  (make 'future-carousel
        :vector (make-array count)))


(defmethod end-execution ((obj future-carousel))
  (map nil #'lparallel:force (read-vector obj)))


(defmethod initialize-instance ((obj future-carousel) &key &allow-other-keys)
  (call-next-method)
  (c2mop:set-funcallable-instance-function
   obj
   (lambda (x)
     (bind (((:slots %vector %index) obj))
       (setf %index (mod (1+ %index) (array-dimension %vector 0)))
       (lparallel:force (aref %vector %index))
       (setf (aref %vector %index) (lparallel:future (funcall x)))))))
