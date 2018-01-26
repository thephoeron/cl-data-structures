(in-package #:cl-data-structures.sequences)


(defclass abstract-sequence ()
  ())


(defmethod cl-ds:make-bucket ((operation cl-ds:grow-function)
                              (container abstract-sequence)
                              location
                              &key value &allow-other-keys)
  value)


(defmethod cl-ds:shrink-bucket ((operation cl-ds:shrink-function)
                                (container abstract-sequence)
                                bucket
                                location
                                &rest rest &key &allow-other-keys)
  (declare (ignore operation container bucket location rest))
  nil)


(defmethod cl-ds:put ((container abstract-sequence) item)
  (cl-ds:position-modification #'cl-ds:put container nil :value item))


(defmethod cl-ds:take-out ((container abstract-sequence))
  (cl-ds:position-modification #'cl-ds:take-out container nil))
