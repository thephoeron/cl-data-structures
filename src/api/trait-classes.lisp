(in-package #:cl-data-structures)


(defclass functional (fundamental-container)
  ()
  (:documentation "Object implements functional api."))


(defclass mutable (fundamental-container)
  ()
  (:documentation "Object implements mutable api."))


(defclass transactional (mutable)
  ()
  (:documentation "Object implements mutable api in transactional way."))
