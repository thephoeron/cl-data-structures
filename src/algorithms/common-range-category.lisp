(in-package #:cl-data-structures.algorithms)


(defgeneric fundamental-range-class-of (range class)
  (:method ((range cl-ds:fundamental-forward-range) class)
    (declare (ignore class))
    'fundamental-forward-range)
  (:method ((range cl-ds:fundamental-bidirectional-range)
            (class (eql 'fundamental-random-access-range)))
    'fundamental-bidirectional-range)
  (:method ((range cl-ds:fundamental-bidirectional-range)
            (class (eql 'fundamental-bidirectional-range)))
    'fundamental-bidirectional-range)
  (:method ((range cl-ds:fundamental-random-access-range)
            class)
    class))


(defun common-fundamental-range-class (ranges)
  (reduce (lambda (prev next) (fundamental-range-class-of next prev))
          ranges
          :initial-value 'fundamental-random-access-range))
