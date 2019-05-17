(cl:in-package #:cl-ds.rf)


(defgeneric make-submodel (class data arguments))
(defgeneric make-submodel-with-model (main-model data arguments))
(defgeneric make-node (main-model data arguments))
(defgeneric submodel-class (main-model))
(defgeneric train (model data))
(defgeneric predict (model data))
(defgeneric tree-count (model))
(defgeneric tree-maximum-depth (model))
(defgeneric submodels (model))
(defgeneric submodel-arguments (model))
(defgeneric split-attempts (model))
