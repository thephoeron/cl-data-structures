(in-package #:cl-data-structures)


(defclass fundamental-container ()
  ())


(defclass fundamental-modification-operation-status ()
  ())


(defclass fundamental-range ()
  ())


(defclass fundamental-assiganable-range (fundamental-range)
  ())


(defclass fundamental-forward-range (fundamental-range)
  ())


(defclass fundamental-bidirectional-range (fundamental-forward-range)
  ())


(defclass fundamental-random-access-range (fundamental-bidirectional-range)
  ())


(defclass fundamental-assignable-forward-range (fundamental-forward-range
                                                fundamental-assiganable-range)
  ())


(defclass fundamental-assignable-bidirectional-range (fundamental-bidirectional-range
                                                      fundamental-assiganable-range)
  ())


(defclass fundamental-assignable-random-access-range (fundamental-random-access-range
                                                      fundamental-assiganable-range)
  ())
