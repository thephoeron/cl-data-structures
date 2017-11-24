(in-package #:cl-data-structures.common)


(defstruct hash-content
  (hash 0 :type fixnum)
  location)


(defstruct (hash-dict-content (:include hash-content))
  value)


(defun single-element-p (seq)
  (and (not (null seq))
       (cond ((listp seq)
              (endp (rest seq)))
             ((vectorp seq)
              (eql (length seq) 1)))))
