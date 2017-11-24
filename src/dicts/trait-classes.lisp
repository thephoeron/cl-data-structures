(in-package #:cl-ds.dicts)


(defclass abstract-dictionary ()
  ((%equal-fn
    :type (-> (t t) boolean)
    :initarg :equal-fn
    :reader read-equal-fn)))


(defclass abstract-hashing-dictionary (abstract-dictionary)
  ((%hash-fn
    :type (-> (t) fixnum)
    :initarg :hash-fn
    :reader read-hash-fn)))


(defclass dictionary (cl-ds:fundamental-container)
  ())


(defclass functional-dictionary (dictionary cl-ds:functional)
  ())


(defclass lazy-dictionary (cl-ds:lazy functional-dictionary)
  ())


(defclass mutable-dictionary (dictionary cl-ds:mutable)
  ())


(defclass transactional-dictionary (mutable-dictionary cl-ds:transactional)
  ())


(defclass hashing-dictionary (dictionary)
  ())


(defclass mutable-hashing-dictionary (hashing-dictionary mutable-dictionary)
  ())


(defclass functional-hashing-dictionary (hashing-dictionary functional-dictionary)
  ())


(defclass transactional-hashing-dictionary (hashing-dictionary transactional-dictionary)
  ())


(defclass lazy-hashing-dictionary (hashing-dictionary lazy-dictionary)
  ())
