(in-package #:cl-ds.dicts)


(defclass dictionary (cl-ds:fundamental-container)
  ((%equal-fn
    :type (-> (t t) boolean)
    :initarg :equal-fn
    :reader read-equal-fn)))


(defclass functional-dictionary (dictionary cl-ds:functional)
  ())


(defclass mutable-dictionary (dictionary cl-ds:mutable)
  ())


(defclass transactional-dictionary (mutable-dictionary cl-ds:transactional)
  ())


(defclass hashing-dictionary (dictionary)
  ((%hash-fn
    :type (-> (t) fixnum)
    :initarg :hash-fn
    :reader read-hash-fn)))


(defclass mutable-hashing-dictionary (hashing-dictionary mutable-dictionary)
  ())


(defclass functional-hashing-dictionary (hashing-dictionary functional-dictionary)
  ())


(defclass transactional-hashing-dictionary (hashing-dictionary transactional-dictionary)
  ())
