(cl:in-package #:cl-data-structures.metric-space)


(defclass metric-space-set (cl-ds:fundamental-container)
  ((%metric-fn :reader read-metric-fn
               :initarg :metric-fn))
  (:metaclass funcallable-standard-class))


(defclass metric-space-dictionary (cl-ds:fundamental-container)
  ()
  (:metaclass funcallable-standard-class))


(defclass mutable-metric-space-set (metric-space-set)
  ()
  (:metaclass funcallable-standard-class))


(defclass mutable-metric-space-dictionary (metric-space-dictionary)
  ()
  (:metaclass funcallable-standard-class))
