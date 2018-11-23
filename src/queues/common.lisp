(in-package #:cl-data-structures.queues)


(defclass fundamental-queue (cl-ds:fundamental-container)
  ((%size :initarg :size
          :initform 0
          :reader cl-ds:size
          :accessor access-size)))


(defclass fundamental-mutable-queue (cl-ds:mutable fundamental-queue)
  ())


(defclass fundamental-transactional-queue (cl-ds:transactional fundamental-queue)
  ())


(defclass fundamental-functional-queue (cl-ds:functional fundamental-queue)
  ())


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:grow-function)
                                   (container fundamental-queue)
                                   location
                                   &rest all)
  (declare (ignore all))
  (values (cl-ds:force location)
          cl-ds.common:empty-eager-modification-operation-status
          t))


(defmethod cl-ds.meta:shrink-bucket ((operation cl-ds.meta:shrink-function)
                                     (container fundamental-functional-queue)
                                     bucket
                                     location
                                     &rest all)
  (declare (ignore location all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds.meta:shrink-bucket ((operation cl-ds.meta:shrink-function)
                                     (container fundamental-transactional-queue)
                                     bucket
                                     location
                                     &rest all)
  (declare (ignore location all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))

(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:shrink-function)
                                      (container fundamental-mutable-queue)
                                      bucket
                                      location
                                      &rest all)
  (declare (ignore location all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:put! ((container fundamental-mutable-queue)
                       item)
  (cl-ds.meta:position-modification #'cl-ds:put!
                                    container
                                    container
                                    item))


(defmethod cl-ds:take-out! ((container fundamental-mutable-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out!
                                    container
                                    container
                                    nil))


(defmethod cl-ds:take-out! ((container fundamental-transactional-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out!
                                    container
                                    container
                                    nil))


(defmethod cl-ds:take-out ((container fundamental-functional-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out
                                    container
                                    container
                                    nil))


(defmethod cl-ds:put ((container fundamental-functional-queue)
                      item)
  (cl-ds.meta:position-modification #'cl-ds:put
                                    container
                                    container
                                    item))


(defmethod cl-ds:take-out! ((container fundamental-functional-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out
                                    container
                                    container
                                    nil))
