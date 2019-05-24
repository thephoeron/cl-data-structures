(cl:in-package #:cl-user)


(defpackage #:cl-data-structures.random-forest
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.rf)
  (:export
   #:encode-data-into-context
   #:encode-data-into-contexts
   #:make-model
   #:make-node
   #:make-submodel
   #:make-submodel-prediction-context
   #:make-submodel-prediction-contexts
   #:make-submodel-with-model
   #:predict
   #:split-attempts
   #:submodel-arguments
   #:submodel-class
   #:submodel-predict
   #:submodels
   #:train
   #:tree-count
   #:tree-maximum-depth
   #:tree-minimal-size
   ))
