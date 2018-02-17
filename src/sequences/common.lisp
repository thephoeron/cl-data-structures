(in-package #:cl-data-structures.sequences)


(defclass fundamental-sequence ()
  ())


(defclass functional-sequence (fundamental-sequence cl-ds:functional)
  ())


(defclass mutable-sequence (fundamental-sequence cl-ds:mutable)
  ())


(defclass transactional-sequence (mutable-sequence cl-ds:transactional)
  ())


(defmethod cl-ds:make-bucket ((operation cl-ds:grow-function)
                              (container fundamental-sequence)
                              location
                              &key value &allow-other-keys)
  (values (cl-ds:force value) cl-ds.common:empty-eager-modification-operation-status t))


(defmethod cl-ds:shrink-bucket! ((operation cl-ds:shrink-function)
                                 (container mutable-sequence)
                                 bucket
                                 location
                                 &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (values nil
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:shrink-bucket ((operation cl-ds:shrink-function)
                                (container fundamental-sequence)
                                bucket
                                location
                                &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (values nil
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket)
          t))


(defmethod cl-ds:grow-bucket! ((operation cl-ds:grow-function)
                               (container mutable-sequence)
                               bucket
                               location
                               &rest rest &key value
                               &allow-other-keys)
  (declare (ignore rest))
  (values (cl-ds:force value)
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
                              (container fundamental-sequence)
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


(defmethod cl-ds:put! ((container mutable-sequence) item)
  (cl-ds:position-modification #'cl-ds:put! container nil :value item))


(defmethod cl-ds:take-out! ((container mutable-sequence))
  (cl-ds:position-modification #'cl-ds:take-out! container nil))


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


(defmethod cl-ds:update-if! ((container functional-sequence)
                             location
                             new-value
                             condition-fn)
  (cl-ds:position-modification #'cl-ds:update-if! container location
                               :value new-value
                               :condition-fn condition-fn))


(defmethod (setf cl-ds:at) (new-value (container mutable-sequence) location)
  (cl-ds:position-modification #'(setf cl-ds:at)
                               container location :value new-value)
  new-value)


(defclass lazy-box-sequence (cl-ds.common:lazy-box-container
                             functional-sequence)
  ())


(defmethod cl-ds:at ((container lazy-box-sequence) location)
  (cl-ds.common:force-version container)
  (cl-ds:at (cl-ds.common:access-content container) location))


(defmethod cl-ds:become-lazy ((container cl-ds.seqs:fundamental-sequence))
  (make 'lazy-box-sequence
        :content (cl-ds:become-transactional container)))


(defmethod cl-ds:whole-range ((container lazy-box-sequence))
  (cl-ds.common:make-lazy-range cl-ds.common:lazy-random-access-range
                                container
                                (cl-ds:whole-range container)))
