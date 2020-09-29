(in-package :cl-ds.dicts)

(defclass fundamental-dictionary (cl-ds:fundamental-container)
  ()
  (:metaclass funcallable-standard-class))

(defclass fundamental-hashing-dictionary (fundamental-dictionary)
  ((%hash-fn
    :type (-> (t) fixnum)
    :initarg :hash-fn
    :reader read-hash-fn)
   (%equal-fn
    :type (-> (t t) boolean)
    :initarg :equal-fn
    :reader read-equal-fn)
   (%bucket-size
    :type positive-fixnum
    :initarg :bucket-size
    :reader read-bucket-size
    :initform 3))
  (:metaclass funcallable-standard-class))

(defclass fundamental-sparse-vector (fundamental-dictionary)
  ()
  (:metaclass funcallable-standard-class))

(defclass functional-sparse-vector (fundamental-sparse-vector)
  ()
  (:metaclass funcallable-standard-class))

(defclass mutable-sparse-vector (fundamental-sparse-vector)
  ()
  (:metaclass funcallable-standard-class))

(defclass transactional-sparse-vector (mutable-sparse-vector)
  ()
  (:metaclass funcallable-standard-class))

(defclass functional-dictionary (fundamental-dictionary cl-ds:functional)
  ()
  (:metaclass funcallable-standard-class))

(defclass lazy-dictionary (cl-ds:lazy functional-dictionary)
  ()
  (:metaclass funcallable-standard-class))

(defclass mutable-dictionary (fundamental-dictionary cl-ds:mutable)
  ()
  (:metaclass funcallable-standard-class))

(defclass transactional-dictionary (mutable-dictionary cl-ds:transactional)
  ()
  (:metaclass funcallable-standard-class))

(defclass mutable-hashing-dictionary (fundamental-hashing-dictionary mutable-dictionary)
  ()
  (:metaclass funcallable-standard-class))

(defclass functional-hashing-dictionary (fundamental-hashing-dictionary functional-dictionary)
  ()
  (:metaclass funcallable-standard-class))

(defclass transactional-hashing-dictionary (fundamental-hashing-dictionary transactional-dictionary)
  ()
  (:metaclass funcallable-standard-class))

(defclass lazy-hashing-dictionary (functional-hashing-dictionary lazy-dictionary)
  ()
  (:metaclass funcallable-standard-class))
