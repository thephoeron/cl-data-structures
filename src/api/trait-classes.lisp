(in-package #:cl-data-structures)


(defclass functional (fundamental-container)
  ())


(defclass mutable (fundamental-container)
  ())


(defclass transactional (mutable)
  ())


(defclass lazy (functional)
  ())
