(in-package #:cl-data-structures.metric-space)


(defmethod cl-ds:shrink-bucket! ((operation cl-ds:erase!-function)
                                 (container (mutable-metric-space-set))
                                 bucket
                                 location)
  cl-ds.utils:todo)


(defmethod cl-ds:shrink-bucket! ((operation cl-ds:erase-if!-function)
                                 (container (mutable-metric-space-set))
                                 bucket
                                 location)
  cl-ds.utils:todo)


(defmethod cl-ds:grow-bucket! ((operation cl-ds:insert!-function)
                               (container (mutable-metric-space-set))
                               bucket
                               location)
  cl-ds.utils:todo)


(defmethod cl-ds:grow-bucket! ((operation cl-ds:add!-function)
                               (container (mutable-metric-space-set))
                               bucket
                               location)
  cl-ds.utils:todo)
