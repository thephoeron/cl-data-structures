(in-package #:cl-data-structures.metric-space)


(defmethod cl-ds:put! ((container mutable-metric-space-set) item)
  (cl-ds.meta:position-modification #'cl-ds:put! container item))


(defmethod cl-ds:erase! ((container mutable-metric-space-set) item)
  (cl-ds.meta:position-modification #'cl-ds:erase! container item))


(defmethod cl-ds:erase-if! ((container mutable-metric-space-set)
                            item condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:erase! container item
                                    :condition-fn condition-fn))
