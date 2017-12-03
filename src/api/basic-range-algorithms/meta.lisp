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


(defgeneric make-state (aggregation-function
                        &rest all
                        &key &allow-other-keys))


(defgeneric aggregate (function state element))


(defgeneric state-result (function state))


(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))


(defgeneric apply-aggregation-function (range function
                                        &rest all &key &allow-other-keys))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defmethod apply-aggregation-function ((range cl-ds:fundamental-range)
                                       (function aggregation-function)
                                       &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range))
        (state (apply #'make-state function all)))
    (iterate
      (while (morep clone))
      (aggregate function state (consume-front clone)))
    (state-result function state)))
