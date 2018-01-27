(in-package #:cl-data-structures.sequences)


(defclass abstract-sequence ()
  ())


(defmethod cl-ds:make-bucket ((operation cl-ds:grow-function)
                              (container abstract-sequence)
                              location
                              &key value &allow-other-keys)
  (values value cl-ds.common:empty-eager-modification-operation-status t))


(defmethod cl-ds:shrink-bucket ((operation cl-ds:shrink-function)
                                (container abstract-sequence)
                                bucket
                                location
                                &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (values nil
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:grow-bucket ((operation cl-ds:grow-function)
                              (container abstract-sequence)
                              bucket
                              location
                              &rest rest &key value
                              &allow-other-keys)
  (declare (ignore rest))
  (values value
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:put ((container abstract-sequence) item)
  (cl-ds:position-modification #'cl-ds:put container nil :value item))


(defmethod cl-ds:take-out ((container abstract-sequence))
  (cl-ds:position-modification #'cl-ds:take-out container nil))


(defmethod cl-ds:update ((container abstract-sequence) location new-value)
  (cl-ds:position-modification #'cl-ds:update container location
                               :value new-value))
