(in-package #:cl-data-structures.queues.2-3-tree)


(define-constant +buffer-size+ 32)


(deftype queue-buffer ()
  `(simple-vector ,+buffer-size+))


(deftype buffer-index ()
  `(integer 0 ,+buffer-size+))


(locally (declare (optimize (debug 3) (safety 3)))
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
                :initform 0))))


(defclass mutable-2-3-queue (2-3-queue
                             cl-ds.queues:fundamental-mutable-queue)
  ())


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:grow-function)
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
    ((operation cl-ds.meta:shrink-function)
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
                cl-ds.utils:todo ; should signal error
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
  cl-ds.utils:todo)


(defmethod cl-ds:at ((container 2-3-queue)
                     (position (eql :back))
                     &rest more)
  (cl-ds:assert-one-dimension more)
  cl-ds.utils:todo)
