(in-package #:cl-data-structures.algorithms)


(defclass forward-cartesian-range (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :type vector
             :reader read-content)
   (%function :initarg :function
              :reader read-function)
   (%original-content :initarg :orginal-content
                      :type vector
                      :reader read-original-content)))


(defmethod cl-ds:clone ((range forward-cartesian-range))
  (make (type-of range)
        :function (read-function range)
        :content (map 'vector #'cl-ds:clone (read-content range))
        :original-content (map 'vector #'cl-ds:clone (read-content range))))


(defun pivot-index (range)
  cl-ds.utils:todo)


(defun cartesian-consume-front-cycle (range)
  cl-ds.utils:todo)


(defun cartesian-peek-front-cycle (range)
  cl-ds.utils:todo)


(defmethod cl-ds:consume-front ((range forward-cartesian-range))
  (bind (((:slots %content) range)
         (pivot-index (pivot-index range))
         (pivot-range (aref %content pivot-index))
         ((:values _ more) (cl-ds:peek-front pivot-range)))
    (if more
        (prog1 (apply (read-function range)
                      (map 'list #'cl-ds:peek-front %content))
          (cl-ds:consume-front pivot-range))
        (cartesian-consume-front-cycle range))))


(defmethod cl-ds:peek-front ((range forward-cartesian-range))
  (bind (((:slots %content) range)
         (pivot-index (pivot-index range))
         (pivot-range (aref %content pivot-index))
         ((:values _ more) (cl-ds:peek-front pivot-range)))
    (if more
        (apply (read-function range) (map 'list #'cl-ds:peek-front %content))
        (cartesian-peek-front-cycle range))))
