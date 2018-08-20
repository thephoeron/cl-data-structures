(in-package #:cl-data-structures.algorithms)


(defclass forward-cartesian-range (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :type vector
             :reader read-content)
   (%function :initarg :function
              :reader read-function)
   (%pivot-index :initform 0
                 :initarg :pivot-index
                 :accessor access-pivot-index)
   (%original-pivot-index :initform 0
                          :initarg :original-pivot-index
                          :reader read-original-pivot-index)))


(defmethod cl-ds:reset! ((range forward-cartesian-range))
  (map nil #'cl-ds:reset! (read-content range))
  (setf (access-pivot-index range) (read-original-pivot-index range))
  range)


(defmethod cl-ds:clone ((range forward-cartesian-range))
  (make (type-of range)
        :function (read-function range)
        :pivot-index (access-pivot-index range)
        :original-pivot-index (access-pivot-index range)
        :content (map 'vector #'cl-ds:clone (read-content range))))


(defun cartesian-consume-front-cycle (range)
  (bind (((:slots %content %pivot-index) range))
    (unless (nth-value 1 (cl-ds:peek-front (aref %content %pivot-index)))
      (incf %pivot-index))
    (if (eql %pivot-index (length %content))
        (progn (setf (access-pivot-index range) (length %content))
               (values nil nil))
        (iterate
          (for i from 0 below %pivot-index)
          (cl-ds:reset! (aref %content i))
          (finally
           (cl-ds:consume-front (aref %content %pivot-index))
           (if (nth-value 1 (cl-ds:peek-front (aref %content %pivot-index)))
               (return (cl-ds:consume-front range))
               (progn
                 (setf (access-pivot-index range) (length %content))
                 (return (values nil nil)))))))))


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
  (if (eql (access-pivot-index range) (~> range read-content length))
      (values nil nil)
      (bind (((:slots %content) range)
             (pivot-range (aref %content 0))
             ((:values _ more) (cl-ds:peek-front pivot-range)))
        (if more
            (multiple-value-prog1
                (values (apply (read-function range)
                               (map 'list #'cl-ds:peek-front %content))
                        t)
              (cl-ds:consume-front pivot-range))
            (cartesian-consume-front-cycle range)))))


(defmethod cl-ds:peek-front ((range forward-cartesian-range))
  (if (eql (access-pivot-index range) (~> range read-content length))
      (values nil nil)
      (bind (((:slots %content) range)
             (pivot-range (aref %content 0))
             ((:values _ more) (cl-ds:peek-front pivot-range)))
        (if more
            (values (apply (read-function range)
                           (map 'list #'cl-ds:peek-front %content))
                    t)
            (cartesian-peek-front-cycle range)))))


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
