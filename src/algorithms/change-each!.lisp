(in-package #:cl-data-structures.algorithms)


(defgeneric change-each! (function range &key key)
  (:generic-function-class transformation!-function))
