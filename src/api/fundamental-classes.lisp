(in-package #:cl-data-structures)


(defclass fundamental-container ()
  ())


(defclass fundamental-modification-operation-status ()
  ())


(defclass fundamental-range ()
  ())


(defclass fundamental-forward-range (fundamental-range)
  ())


(defclass fundamental-backward-range (fundamental-range)
  ())


(defclass fundamental-bidirectional-range (fundamental-forward-range
                                           fundamental-backward-range)
  ())


(defclass fundamental-random-access-range (fundamental-bidirectional-range)
  ())
