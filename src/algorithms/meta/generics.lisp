(cl:in-package #:cl-data-structures.algorithms.meta)

#|
Top level aggregator protocol.
|#

(defgeneric pass-to-aggregation (aggregator element))

(defgeneric construct-aggregator (range key function outer-fn arguments))

(defgeneric extract-result (aggregator)
  (:method ((aggregator abstract-proxy-aggregator))
    (cl-ds:forward-call aggregator #'extract-result)))

#|
Range function invokaction protocol.
|#

(defgeneric apply-layer (range function &rest all &key &allow-other-keys))

(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))

(defgeneric apply-aggregation-function (range function
                                        &rest all
                                        &key key
                                        &allow-other-keys))

(defgeneric apply-aggregation-function-with-aggregator
    (aggregator range function
     &rest all
     &key key
     &allow-other-keys))

(defgeneric make-state (aggregation-function
                        &rest all
                        &key &allow-other-keys))

(defgeneric aggregate (function state element))

(defgeneric state-result (function state)
  (:method ((function aggregation-function) state)
    state))

(defgeneric across-aggregate (range function)
  (:method ((range cl-ds:traversable) function)
    (cl-ds:across range function))
  (:method ((range sequence) function)
    (map nil function range)
    range))
