(in-package :cl-data-structures)

(defclass functional (fundamental-container)
  ()
  (:metaclass funcallable-standard-class))

(defclass mutable (fundamental-container)
  ()
  (:metaclass funcallable-standard-class))

(defclass transactional (mutable)
  ()
  (:metaclass funcallable-standard-class))

(defclass lazy (functional)
  ()
  (:metaclass funcallable-standard-class))
