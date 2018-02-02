(in-package #:cl-data-structures.sequences)


(defclass abstract-sequence ()
  ())


(defclass functional-sequence (abstract-sequence cl-ds:functional)
  ())


(defclass mutable-sequence (abstract-sequence cl-ds:mutable)
  ())


(defmethod cl-ds:make-bucket ((operation cl-ds:grow-function)
                              (container abstract-sequence)
                              location
                              &key value &allow-other-keys)
  (values (cl-ds:force value) cl-ds.common:empty-eager-modification-operation-status t))


(defmethod cl-ds:shrink-bucket ((operation cl-ds:shrink-function)
                                (container functional-sequence)
                                bucket
                                location
                                &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (values nil
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:grow-bucket ((operation cl-ds:grow-function)
                              (container functional-sequence)
                              bucket
                              location
                              &rest rest &key value
                              &allow-other-keys)
  (declare (ignore rest))
  (values (cl-ds:force value)
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:grow-bucket ((operation cl-ds:update-if-function)
                              (container abstract-sequence)
                              bucket
                              location
                              &rest rest &key value condition-fn
                              &allow-other-keys)
  (declare (ignore rest))
  (if (funcall condition-fn bucket)
      (values (cl-ds:force value)
              (cl-ds.common:make-eager-modification-operation-status t
                                                                     bucket)
              t)
      (values bucket
              cl-ds.common:empty-eager-modification-operation-status
              nil)))


(defmethod cl-ds:put ((container functional-sequence) item)
  (cl-ds:position-modification #'cl-ds:put container nil :value item))


(defmethod cl-ds:take-out ((container functional-sequence))
  (cl-ds:position-modification #'cl-ds:take-out container nil))


(defmethod cl-ds:update ((container functional-sequence) location new-value)
  (cl-ds:position-modification #'cl-ds:update container location
                               :value new-value))


(defmethod cl-ds:update-if ((container functional-sequence)
                            location
                            new-value
                            condition-fn)
  (cl-ds:position-modification #'cl-ds:update-if container location
                               :value new-value
                               :condition-fn condition-fn))