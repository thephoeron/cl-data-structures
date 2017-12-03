(in-package #:cl-data-structures)


(defgeneric apply-layer (range function &rest all &key &allow-other-keys))


(defclass range-function (closer-mop:standard-generic-function)
  ())


(defclass layer-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric make-state (aggregation-function))


(defgeneric aggregate (function state element))


(defgeneric state-result (function state))


(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))


(defgeneric apply-aggregation-function (range function
                                        &rest all &key &allow-other-keys))



(defclass accumulate-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))
