(in-package #:cl-data-structures.sequences)


(defclass fundamental-sequence ()
  ())


(defclass functional-sequence (fundamental-sequence cl-ds:functional)
  ())


(defclass mutable-sequence (fundamental-sequence cl-ds:mutable)
  ())


(defclass transactional-sequence (mutable-sequence cl-ds:transactional)
  ())


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:grow-function)
                                   (container fundamental-sequence)
                                   location
                                   &key value &allow-other-keys)
  (values (cl-ds:force value)
          cl-ds.common:empty-changed-eager-modification-operation-status
          t))


(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:shrink-function)
                                      (container mutable-sequence)
                                      bucket
                                      location
                                      &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (values 'cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)
          t))


(defmethod cl-ds.meta:shrink-bucket ((operation cl-ds.meta:shrink-function)
                                     (container fundamental-sequence)
                                     bucket
                                     location
                                     &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (values 'cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)
          t))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:grow-function)
                                    (container mutable-sequence)
                                    bucket
                                    location
                                    &rest rest &key value
                                    &allow-other-keys)
  (declare (ignore rest))
  (values (cl-ds:force value)
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)
          t))

(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:grow-function)
                                   (container functional-sequence)
                                   bucket
                                   location
                                   &rest rest &key value
                                   &allow-other-keys)
  (declare (ignore rest))
  (values (cl-ds:force value)
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)
          t))

(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:grow-function)
                                   (container transactional-sequence)
                                   bucket
                                   location
                                   &rest rest &key value
                                   &allow-other-keys)
  (declare (ignore rest))
  (values (cl-ds:force value)
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)
          t))

(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:update-if-function)
                                   (container fundamental-sequence)
                                   bucket
                                   location
                                   &rest rest &key value condition-fn
                                   &allow-other-keys)
  (declare (ignore rest))
  (if (funcall condition-fn bucket)
      (values (cl-ds:force value)
              (cl-ds.common:make-eager-modification-operation-status t
                                                                     bucket
                                                                     t)
              t)
      (values bucket
              cl-ds.common:empty-eager-modification-operation-status
              nil)))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:put-back!-function)
                                             (structure mutable-sequence)
                                             container
                                             location
                                             &key value)
  (cl-ds.meta:position-modification #'cl-ds:put! structure container nil :value value))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:functional-put-back-function)
                                             (structure functional-sequence)
                                             container
                                             location
                                             &key value)
  (cl-ds.meta:position-modification #'cl-ds:put structure container nil :value value))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:functional-take-out-back-function)
                                             (structure functional-sequence)
                                             container
                                             location
                                             &key value)
  (cl-ds.meta:position-modification #'cl-ds:take-out structure container nil :value value))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:take-out-back!-function)
                                             (structure mutable-sequence)
                                             container
                                             location
                                             &key value)
  (cl-ds.meta:position-modification #'cl-ds:take-out! structure container nil :value value))


(defmethod cl-ds:put-back ((container functional-sequence) item)
  (cl-ds.meta:position-modification #'cl-ds:put container container nil :value item))


(defmethod cl-ds:put-back! ((container mutable-sequence) item)
  (cl-ds.meta:position-modification #'cl-ds:put! container container nil :value item))


(defmethod cl-ds:put ((container functional-sequence) item)
  (cl-ds.meta:position-modification #'cl-ds:put container container nil :value item))


(defmethod cl-ds:put! ((container mutable-sequence) item)
  (cl-ds.meta:position-modification #'cl-ds:put! container container nil :value item))


(defmethod cl-ds:take-out! ((container mutable-sequence))
  (cl-ds.meta:position-modification #'cl-ds:take-out! container container nil))


(defmethod cl-ds:take-out ((container functional-sequence))
  (cl-ds.meta:position-modification #'cl-ds:take-out container container nil))


(defmethod cl-ds:take-out-back ((container functional-sequence))
  (cl-ds.meta:position-modification #'cl-ds:take-out container container nil))


(defmethod cl-ds:take-out-back! ((container mutable-sequence))
  (cl-ds.meta:position-modification #'cl-ds:take-out! container container nil))


(defmethod cl-ds:update ((container functional-sequence) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update container container location
                                    :value new-value))


(defmethod cl-ds:update-if ((container functional-sequence)
                            location
                            new-value
                            condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if
                                    container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod cl-ds:update-if! ((container functional-sequence)
                             location
                             new-value
                             condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if!
                                    container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod (setf cl-ds:at) (new-value (container mutable-sequence)
                            location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (cl-ds.meta:position-modification #'(setf cl-ds:at)
                                    container container location
                                    :value new-value)
  new-value)


(defclass lazy-box-sequence (cl-ds.common:lazy-box-container
                             functional-sequence)
  ())


(defmethod cl-ds:at ((container lazy-box-sequence) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (cl-ds.common:force-version container)
  (cl-ds:at (cl-ds.common:access-content container) location))


(defmethod cl-ds:become-lazy ((container cl-ds.seqs:fundamental-sequence))
  (make 'lazy-box-sequence
        :content (cl-ds:become-transactional container)))


(defmethod cl-ds:whole-range ((container lazy-box-sequence))
  (cl-ds.common:make-lazy-range cl-ds.common:lazy-random-access-range
                                container
                                (cl-ds:whole-range container)))
