(in-package #:cl-data-structures.data-frame)


(defgeneric plane (data &rest what))
(defgeneric combine! (target dimension key &rest data))
(defgeneric range-combine! (target dimension range &key key))
(defgeneric mutate! (data dimension function)
  (:method :around (data dimension function)
    (let ((*active-frame* data))
      (call-next-method))))
