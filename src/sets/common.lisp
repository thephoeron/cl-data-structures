(cl:in-package #:cl-data-structures.sets)


(defclass fundamental-set (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))


(defclass mutable-set (fundamental-set cl-ds:mutable)
  ()
  (:metaclass funcallable-standard-class))


(defmethod cl-ds:put! ((container mutable-set) item)
  (cl-ds.meta:position-modification #'cl-ds:put!
                                    container
                                    container
                                    item))


(defmethod cl-ds:erase! ((container mutable-set) location)
  (cl-ds.meta:position-modification #'cl-ds:erase!
                                    container
                                    container
                                    location))
