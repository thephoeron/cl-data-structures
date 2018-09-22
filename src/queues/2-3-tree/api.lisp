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
                   :initform (1- +buffer-size+))
   (%tail :accessor access-tail
          :initform nil
          :initarg :tail)
   (%tail-position :accessor access-tail-position
                   :initarg :tail-position
                   :initform 0)))


(defclass mutable-2-3-queue (2-3-queue
                             cl-ds.queues:fundamental-mutable-queue)
  ())


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:grow-function)
     (structure mutable-2-3-queue)
     container
     location
     &rest all)
  (declare (ignore all))
  (let ((head-position (access-head-position structure))
        (head (ensure (access-head structure)
                #1=(make-array +buffer-size+
                               :element-type (read-element-type structure)))))
    (declare (type buffer-index head-position)
             (type queue-buffer head))
    (if (zerop head-position)
        (progn
          (cl-ds.common.2-3:insert-front-into-tree!
           structure
           (lambda () head))
          (setf (access-head-position structure) (- +buffer-size+ 2)
                head #1#
                (access-head structure) head
                (last-elt head) (cl-ds.meta:make-bucket operation
                                                        container
                                                        head))
          (incf (cl-ds.queues:access-size structure))
          (values structure
                  cl-ds.common:empty-eager-modification-operation-status))
        (progn
          (setf (aref head head-position) (cl-ds.meta:make-bucket operation
                                                                  container
                                                                  location)
                (access-head-position structure) (1- head-position))
          (incf (cl-ds.queues:access-size structure))
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
        (tail (access-tail structure)))
    (declare (type buffer-index tail-position)
             (type queue-buffer tail))
    (if (zerop tail-position)
        (if (cl-ds.meta:null-bucket-p root)
            cl-ds.utils:todo
            cl-ds.utils:todo)
        (bind ((first (aref tail tail-position))
               ((:values new-buffer status _)
                (apply #'cl-ds.meta:shrink-bucket!
                       operation container
                       first location all)))
          (setf (first-elt tail) new-buffer)
          (when (cl-ds.meta:null-bucket-p new-buffer)
            (decf (cl-ds.queues:access-size structure))
            (incf (access-tail-position structure)))
          (values structure status)))))


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
