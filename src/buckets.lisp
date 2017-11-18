(in-package #:cl-data-structures.common)


(defstruct hash-content
  (hash 0 :type fixnum)
  location)

(defstruct (hash-dict-content (:include hash-content))
  value)
