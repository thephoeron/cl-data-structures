(in-package #:cl-data-structures.metric-space)


(defmethod cl-ds:put! ((container mutable-metric-space-set) item)
  (cl-ds.meta:position-modification #'cl-ds:put! container container item))


(defmethod cl-ds:erase! ((container mutable-metric-space-set) item)
  (cl-ds.meta:position-modification #'cl-ds:erase! container container item))
