(cl:in-package #:cl-data-structures.metric-space)


(defclass metric-space-set (cl-ds:fundamental-container)
  ((%metric-fn :reader read-metric-fn
               :initarg :metric-fn)))


(defclass metric-space-dictionary (cl-ds:fundamental-container)
  ())


(defclass mutable-metric-space-set (metric-space-set)
  ())


(defclass mutable-metric-space-dictionary (metric-space-dictionary)
  ())
