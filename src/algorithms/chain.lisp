(in-package #:cl-data-structures.algorithms)


(defclass abstract-chain-of-ranges (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :initform (make 'flexichain:standard-flexichain)
             :reader read-content)
   (%original-content :initarg :original-content
                      :type list
                      :reader read-original-content)))


(defgeneric chain (first-range &rest other-ranges))
