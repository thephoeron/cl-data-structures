(in-package #:cl-data-structures)


(defclass traversable ()
  ())


(defclass fundamental-container (traversable)
  ())


(defclass fundamental-modification-operation-status ()
  ())


(defclass fundamental-range (traversable)
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


(defclass key-value-range ()
  ())


(defclass chunked-range (fundamental-forward-range)
  ((%original-range :initarg :original-range
                    :reader read-original-range)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)))


(defclass chunking-mixin ()
  ())
