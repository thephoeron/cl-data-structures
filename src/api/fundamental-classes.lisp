(cl:in-package #:cl-data-structures)


(defclass traversable (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-container (traversable)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-modification-operation-status (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-range (traversable)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-assiganable-range (fundamental-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-forward-range (fundamental-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-bidirectional-range (fundamental-forward-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-random-access-range (fundamental-bidirectional-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-assignable-forward-range (fundamental-forward-range
                                                fundamental-assiganable-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-assignable-bidirectional-range (fundamental-bidirectional-range
                                                      fundamental-assiganable-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass fundamental-assignable-random-access-range (fundamental-random-access-range
                                                      fundamental-assiganable-range)
  ()
  (:metaclass funcallable-standard-class))


(defclass key-value-range (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))


(defclass chunking-mixin (funcallable-standard-object)
  ()
  (:metaclass funcallable-standard-class))


(defclass chunked-range (chunking-mixin fundamental-forward-range)
  ((%original-range :initarg :original-range
                    :reader read-original-range)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size))
  (:metaclass funcallable-standard-class))
