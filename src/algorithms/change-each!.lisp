(in-package #:cl-data-structures.algorithms)


(defgeneric change-each! (function range)
  (:generic-function-class transformation!-function))
