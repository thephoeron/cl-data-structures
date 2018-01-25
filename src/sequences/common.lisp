(in-package #:cl-data-structures.sequences)


(defclass abstract-sequence ()
  ())


(defmethod cl-ds:make-bucket ((operation cl-ds:grow-function)
                              (container abstract-sequence)
                              location
                              &key value &allow-other-keys)
  value)


(defmethod cl-ds:put ((container abstract-sequence) item)
  (cl-ds:position-modification #'cl-ds:put container nil :value item))
