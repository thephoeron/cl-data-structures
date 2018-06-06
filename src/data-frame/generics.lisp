(in-package #:cl-data-structures.data-frame)


(defgeneric row (data number))
(defgeneric column (data number))
(defgeneric plane (data &rest what))
(defgeneric combine! (target &rest data))
(defgeneric range-combine! (target range))
