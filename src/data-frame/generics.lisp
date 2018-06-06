(in-package #:cl-data-structures.data-frame)


(defgeneric plane (data &rest what))
(defgeneric combine! (target dimension key &rest data))
(defgeneric range-combine! (target dimension range &key key))
(defgeneric mutate! (data function))
