(in-package #:cl-data-structures.algorithms)


(defclass forward-cartesian-range (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :type vector
             :reader read-content)
   (%function :initarg :function
              :reader read-function)
   (%pivot-index :initform 0
                 :accessor access-pivot-index)
   (%finished :initform nil
              :accessor access-finished)
   (%original-content :initarg :orginal-content
                      :type vector
                      :reader reader-original-content)))


(defmethod cl-ds:clone ((range forward-cartesian-range))
  (make (type-of range)
        :function (read-function range)
        :finished (access-finished range)
        :pivot-index (access-pivot-index range)
        :content (map 'vector #'cl-ds:clone (read-content range))
        :original-content (map 'vector #'cl-ds:clone (read-content range))))


(defun cartesian-consume-front-cycle (range)
  (bind (((:slots %content %pivot-index) range))
    (unless (nth-value 1 (cl-ds:peek-front (aref %content %pivot-index)))
      (incf %pivot-index))
    (iterate
      (for i from 0 below %pivot-index)
      (cl-ds:reset! (aref %content i)))
    (if (eql %pivot-index (length %content))
        (progn (setf (access-finished range) t)
               (values nil nil))
        (progn
          (cl-ds:consume-front (aref %content %pivot-index))
          (cl-ds:consume-front range)))))


(defun cartesian-peek-front-cycle (range)
  (bind (((:slots %content %pivot-index) range)
         (content (copy-array %content))
         (pivot-index %pivot-index))
    (unless (nth-value 1 (cl-ds:peek-front (aref content pivot-index)))
      (incf pivot-index))
    (iterate
      (for i from 0 below pivot-index)
      (cl-ds:reset! (aref content i)))
    (if (eql pivot-index (length content))
        (values nil nil)
        (progn
          (setf #1=(aref content pivot-index) (cl-ds:clone #1#))
          (cl-ds:consume-front (aref content pivot-index))
          (values (apply (read-function range) (map 'list #'cl-ds:peek-front content))
                  t)))))


(defmethod cl-ds:consume-front ((range forward-cartesian-range))
  (if (access-finished range)
      (values nil nil)
      (bind (((:slots %content) range)
             (pivot-range (aref %content 0))
             ((:values _ more) (cl-ds:peek-front pivot-range)))
        (if more
            (prog1 (values (apply (read-function range)
                                  (map 'list #'cl-ds:peek-front %content))
                           t)
              (cl-ds:consume-front pivot-range))
            (cartesian-consume-front-cycle range)))))


(defmethod cl-ds:peek-front ((range forward-cartesian-range))
  (if (access-finished range)
      (values nil nil)
      (bind (((:slots %content) range)
             (pivot-range (aref %content 0))
             ((:values _ more) (cl-ds:peek-front pivot-range)))
        (if more
            (values (apply (read-function range)
                           (map 'list #'cl-ds:peek-front %content))
                    t)
            (cartesian-peek-front-cycle range)))))
