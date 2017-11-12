(in-package #:cl-data-structures.dicts)


(defmethod cl-ds:add ((container functional-dictionary) location new-value)
  (cl-ds:position-modification #'cl-ds:add container location :value new-value))


(defmethod cl-ds:add! ((container mutable-dictionary) location new-value)
  (cl-ds:position-modification #'cl-ds:add! container location :value new-value))


(defmethod cl-ds:insert ((container functional-dictionary) location new-value)
  (cl-ds:position-modification #'cl-ds:insert container location :value new-value))


(defmethod cl-ds:erase ((container functional-dictionary) location)
  (cl-ds:position-modification #'cl-ds:erase container location))


(defmethod cl-ds:erase-if ((container functional-dictionary) location condition-fn)
  (cl-ds:position-modification #'cl-ds:erase-if container location :condition-fn condition-fn))


(defmethod cl-ds:erase-if! ((container mutable-dictionary) location condition-fn)
  (cl-ds:position-modification #'cl-ds:erase-if! container location :condition-fn condition-fn))


(defmethod cl-ds:erase! ((container mutable-dictionary) location)
  (cl-ds:position-modification #'cl-ds:erase! container location))


(defmethod cl-ds:update ((container functional-dictionary) location new-value)
  (cl-ds:position-modification #'cl-ds:update container location :value new-value))


(defmethod cl-ds:update! ((container mutable-dictionary) location new-value)
  (cl-ds:position-modification #'cl-ds:update! container location :value new-value))


(defmethod (setf cl-ds:at) (new-value (container mutable-dictionary) location)
  (let ((status (nth-value
                 1
                 (cl-ds:position-modification #'(setf cl-ds:at)
                                              container
                                              location
                                              :value new-value))))
    (values new-value status)))
