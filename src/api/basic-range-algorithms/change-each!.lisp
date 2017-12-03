(in-package #:cl-data-structures)


(defgeneric change-each! (function range)
  (:generic-function-class transformation!-function))
