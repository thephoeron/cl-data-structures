(in-package #:cl-data-structures.data-frame)


(defgeneric plane (data &rest what))
(defgeneric combine! (target dimension key &rest data))
(defgeneric range-combine! (target dimension range &key key))
(defgeneric mutate! (data dimension function &rest ranges)
  (:method :around (data dimension function &rest ranges)
    (let ((*active-frame* data))
      (call-next-method))))
