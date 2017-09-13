(in-package #:cl-ds.dicts)


(defclass dictionary (cl-ds:fundamental-container)
  ((%compare-fn
    :type (-> (t t) boolean)
    :initarg :compare-fn
    :reader read-compare-fn)))


(defclass hashing-dictionary (dictionary)
  ((%hash-fn
    :type (-> (t) fixnum)
    :initarg :hash-fn
    :reader read-hash-fn)))

