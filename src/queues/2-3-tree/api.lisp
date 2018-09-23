(in-package #:cl-data-structures.queues.2-3-tree)


(define-constant +buffer-size+ 32)


(deftype queue-buffer ()
  `(simple-vector ,+buffer-size+))


(deftype buffer-index ()
  `(integer 0 ,+buffer-size+))


(defclass 2-3-queue (cl-ds.common.2-3:tree)
  ((%element-type :initarg :element-type
                  :initform t
                  :reader read-element-type)
   (%head :accessor access-head
          :initform nil
          :initarg :head)
   (%head-position :accessor access-head-position
                   :initarg :head-position
                   :type buffer-index
                   :initform 0)
   (%tail :accessor access-tail
          :initform nil
          :initarg :tail)
   (%tail-position :accessor access-tail-position
                   :initarg :tail-position
                   :type buffer-index
                   :initform 0)
   (%tail-end :accessor access-tail-end
              :initarg :tail-end
              :type buffer-index
              :initform 0)))


(defclass mutable-2-3-queue (2-3-queue
                             cl-ds.queues:fundamental-mutable-queue)
  ())


(defclass functional-2-3-queue (2-3-queue
                                cl-ds.queues:fundamental-mutable-queue)
  ())


(defmethod cl-ds:become-functional ((container mutable-2-3-queue))
  (make 'functional-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :head (access-head container)
        :element-type (read-element-type container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-mutable ((container functional-2-3-queue))
  (make 'mutable-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :head (access-head container)
        :element-type (read-element-type container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defun make-mutable-2-3-queue (&key (element-type t))
  (make 'mutable-2-3-queue :element-type element-type))


(defun make-functional-2-3-queue (&key (element-type t))
  (make 'functional-2-3-queue :element-type element-type))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:put-function)
     (structure functional-2-3-queue)
     container
     location
     &rest all)
  (let ((head-position (access-head-position structure))
        (size (cl-ds.queues:access-size structure))
        (head (access-head structure))
        (element-type (read-element-type structure)))
    (declare (type buffer-index head-position)
             (type queue-buffer head))
    (cond ((eql head-position +buffer-size+)
           (bind ((new-root (cl-ds.common.2-3:insert-front-into-tree
                             structure
                             (lambda () head)))
                  (head (make-array
                             +buffer-size+
                             :element-type element-type))
                  (head-position 1)
                  (size (1+ size)))
             (setf (aref head 0) (apply #'cl-ds.meta:make-bucket
                                        operation
                                        container
                                        location
                                        all))
             (values (make (type-of structure)
                           :head head
                           :size size
                           :element-type element-type
                           :head-position head-position
                           :tail (access-tail structure)
                           :tail-position (access-tail-position structure)
                           :root new-root
                           :tail-end (access-tail-end structure))
                     cl-ds.common:empty-eager-modification-operation-status)))
          (t (bind ((head (copy-array head))
                    (size (1+ size)))
               (setf (aref head head-position) (cl-ds.meta:make-bucket operation
                                                                       container
                                                                       location))
               (values (make (type-of structure)
                             :head head
                             :element-type element-type
                             :size size
                             :head-position (1+ head-position)
                             :tail (access-tail structure)
                             :tail-position (access-tail-position structure)
                             :root (cl-ds.common.2-3:access-root structure)
                             :tail-end (access-tail-end structure))
                       cl-ds.common:empty-eager-modification-operation-status)
               (values structure
                       cl-ds.common:empty-eager-modification-operation-status))))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:put!-function)
     (structure mutable-2-3-queue)
     container
     location
     &rest all)
  (let ((head-position (access-head-position structure))
        (size (cl-ds.queues:access-size structure))
        (head (ensure (access-head structure)
                #1=(make-array +buffer-size+
                               :element-type (read-element-type structure)))))
    (declare (type buffer-index head-position)
             (type queue-buffer head))
    (cond ((eql head-position +buffer-size+)
           (cl-ds.common.2-3:insert-front-into-tree! structure
                                                     (lambda () head))
           (setf (access-head-position structure) 1
                 head #1#
                 (access-head structure) head
                 (aref head 0) (apply #'cl-ds.meta:make-bucket
                                      operation
                                      container
                                      location
                                      all)
                 (cl-ds.queues:access-size structure) (1+ size))
           (values structure
                   cl-ds.common:empty-eager-modification-operation-status))
          (t (setf (aref head head-position) (cl-ds.meta:make-bucket operation
                                                                     container
                                                                     location)
                   (access-head-position structure) (1+ head-position)
                   (cl-ds.queues:access-size structure) (1+ size))
             (values structure
                     cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:take-out!-function)
     (structure mutable-2-3-queue)
     container
     location
     &rest all)
  (let ((tail-position (access-tail-position structure))
        (root (cl-ds.common.2-3:access-root structure))
        (tail (access-tail structure))
        (tail-end (access-tail-end structure)))
    (declare (type buffer-index tail-position)
             (type (or null queue-buffer) tail))
    (if (< tail-position tail-end)
        (bind ((bucket (aref tail tail-position))
               ((:values new-buffer status changed)
                (apply #'cl-ds.meta:shrink-bucket!
                       operation container
                       bucket location all)))
          (if (cl-ds.meta:null-bucket-p new-buffer)
              (progn
                (decf (cl-ds.queues:access-size structure))
                (when (eql +buffer-size+ (incf (access-tail-position structure)))
                  (setf (access-tail-position structure) +buffer-size+)))
              (setf (aref tail tail-position) new-buffer))
          (values structure status))
        (if (cl-ds.meta:null-bucket-p root)
            (if (~> structure cl-ds:size zerop)
                (error 'cl-ds:operation-not-allowed
                       :text "Can't reduce size of the empty queue!")
                (bind ((head (access-head structure))
                       (bucket (aref head 0))
                       (head-position (access-head-position structure))
                       ((:values shrinked-bucket status)
                        (apply #'cl-ds.meta:shrink-bucket!
                               operation
                               container
                               bucket
                               location
                               all)))
                  (setf (access-tail-end structure) head-position
                        (access-head-position structure) 0
                        (access-head structure) nil
                        (access-tail structure) head
                        (access-tail-position structure) 0)
                  (if (cl-ds.meta:null-bucket-p shrinked-bucket)
                      (progn
                        (decf (cl-ds.queues:access-size structure))
                        (when (eql head-position (incf (access-tail-position structure)))
                          (setf (access-tail-position structure) -1)))
                      (setf (aref head 0) shrinked-bucket))
                  (values structure status)))
            (bind (((:values _ buffer)
                    (cl-ds.common.2-3:delete-back-from-tree! structure))
                   ((:values new-bucket status changed)
                    (apply #'cl-ds.meta:shrink-bucket!
                           operation
                           container
                           (aref buffer 0)
                           location
                           all)))
              (setf (access-tail-position structure) 0
                    (access-tail-end structure) +buffer-size+
                    (access-tail structure) buffer)
              (if (cl-ds.meta:null-bucket-p new-bucket)
                  (progn
                    (decf (cl-ds.queues:access-size structure))
                    (incf (access-tail-position structure)))
                  (setf (aref buffer 0) new-bucket))
              (values structure status))))))


(defmethod cl-ds:at ((container 2-3-queue)
                     (position (eql :front))
                     &rest more)
  (cl-ds:assert-one-dimension more)
  (when (~> container cl-ds:size zerop)
    (return-from cl-ds:at (values nil nil)))
  (let ((tail (access-tail container))
        (tail-position (access-tail-position container))
        (tail-end (access-tail-end container)))
    (if (eql tail-position tail-end)
        (iterate
          (for node initially (cl-ds.common.2-3:access-root container)
               then (cl-ds.common.2-3:access-right node))
          (while (typep node 'cl-ds.common.2-3:node))
          (finally
           (return (values (aref (the queue-buffer node) 0)
                           t))))
        (values (aref tail tail-position) t))))


(defmethod cl-ds:at ((container 2-3-queue)
                     (position (eql :back))
                     &rest more)
  (cl-ds:assert-one-dimension more)
  (when (~> container cl-ds:size zerop)
    (return-from cl-ds:at (values nil nil)))
  (let ((head (access-head container))
        (head-position (access-head-position container)))
    (if (or (null head) (zerop head-position))
        (iterate
          (for node initially (cl-ds.common.2-3:access-root container)
               then (cl-ds.common.2-3:access-left node))
          (while (typep node 'cl-ds.common.2-3:node))
          (finally
           (return (values (aref (the queue-buffer node) #.(1- +buffer-size+))
                           t))))
        (values (aref head (1- head-position)) t))))
