(cl:in-package #:cl-data-structures.algorithms.meta)

#|
Top level aggregator protocol.
|#

(defgeneric expects-content-p (aggregator))

(defgeneric pass-to-aggregation (aggregator element))

(defgeneric construct-aggregator (range key function outer-fn arguments))

(defgeneric begin-aggregation (aggregator))

(defgeneric end-aggregation (aggregator))

(defgeneric extract-result (aggregator))

(defgeneric aggregator-finished-p (aggregator))

#|
Stage level aggregator protocol.
|#

(defgeneric expects-content-with-stage-p (stage aggregator))

(defgeneric initialize-stage (stage arguments))

(defgeneric pass-to-aggregation-with-stage (stage aggregator element))

(defgeneric extract-result-with-stage (stage aggregator))

(defgeneric begin-aggregation-with-stage (stage aggregator))

(defgeneric end-aggregation-with-stage (stage aggregator))

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

(defgeneric multi-aggregation-stages (aggregation-function
                                      &rest all &key &allow-other-keys)
  (:method ((function aggregation-function) &rest all &key &allow-other-keys)
    (declare (ignore all))
    nil))

(defgeneric make-state (aggregation-function
                        &rest all
                        &key &allow-other-keys))

(defgeneric aggregate (function state element))

(defgeneric state-result (function state)
  (:method ((function aggregation-function) state)
    state))

(defgeneric across-aggregate (function range)
  (:method (function (range cl-ds:traversable))
    (cl-ds:across function range)))
