(in-package #:cl-data-structures.algorithms)


(defclass forward-cartesian-range (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :type vector
             :reader read-content)
   (%function :initarg :function
              :reader read-function)))


(defmethod cl-ds:reset! ((range forward-cartesian-range))
  (map nil #'cl-ds:reset! (read-content range))
  (setf (access-pivot-index range) (read-original-pivot-index range))
  range)


(defmethod cl-ds:clone ((range forward-cartesian-range))
  (make (type-of range)
        :function (read-function range)
        :content (map 'vector #'cl-ds:clone (read-content range))))


(defmethod cl-ds:consume-front ((range forward-cartesian-range))
  (bind ((content (read-content range))
         (data (map 'list (lambda (x) (multiple-value-list (cl-ds:peek-front x)))
                    content))
         (more (every #'second data)))
    (if more
        (let ((result (apply (read-function range) (mapcar #'first data))))
          (iterate
            (for i from 0 below (length content))
            (for c in-vector content)
            (cl-ds:consume-front c)
            (when (nth-value 1 (cl-ds:peek-front c))
              (iterate
                (for j from 0 below i)
                (for c in-vector content)
                (cl-ds:reset! c))
              (finish)))
          (values result t))
        (values nil nil))))


(defmethod cl-ds:peek-front ((range forward-cartesian-range))
  (bind ((content (read-content range))
         (data (map 'list (lambda (x) (multiple-value-list (cl-ds:peek-front x)))
                    content))
         (more (every #'second data)))
    (if more
        (let ((result (apply (read-function range) (mapcar #'first data))))
          (values result t))
        (values nil nil))))


(defun cartesian (function range &rest more-ranges)
  (declare (dynamic-extent more-ranges))
  (check-type range fundamental-range)
  (iterate
    (for r in more-ranges)
    (check-type r fundamental-range))
  (make 'forward-cartesian-range
        :function function
        :content (~> (cons range more-ranges)
                     (coerce 'vector))))
