(in-package #:cl-ds.dicts)


(defclass dictionary (cl-ds:fundamental-container)
  ((%equal-fn
    :type (-> (t t) boolean)
    :initarg :equal-fn
    :reader read-equal-fn)))


(defclass hashing-dictionary (dictionary)
  ((%hash-fn
    :type (-> (t) fixnum)
    :initarg :hash-fn
    :reader read-hash-fn)))

