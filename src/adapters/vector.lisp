(in-package #:cl-data-structures.adapters)


(defclass vector-range (cl-ds:fundamental-random-access-range)
  ((%vector
    :type vector
    :initarg :vector
    :reader read-vector)
   (%lower-bound
    :type fixnum
    :accessor access-lower-bound
    :initform 0)
   (%upper-bound
    :type fixnum
    :accessor access-upper-bound)))


(defun init-vector-range (range)
  (bind (((:slots %upper-bound %lower-bound %vector) range))
    (setf %lower-bound 0
          %upper-bound (length %vector)))
  range)


(defmethod reinitialize-instance ((range vector-range) &key &allow-other-keys)
  (init-vector-range range))


(defmethod initialize-instance :after ((range vector-range) &key &allow-other-keys)
  (init-vector-range range))


(defmethod cl-ds:whole-range ((obj vector))
  (make 'vector-range :vector obj))


(defmethod cl-ds:at ((range vector-range) location)
  (bind (((:slots %vector %lower-bound %upper-bound) range))
    (if (and (>= %lower-bound location) (< %upper-bound location))
        (values (elt %vector location) t)
        (values nil nil))))


(defmethod cl-ds:reset! ((range vector-range))
  (reinitialize-instance range))


(defun range-not-fully-read (range)
  (bind (((:slots %upper-bound %lower-bound) range))
    (< %lower-bound %upper-bound)))


(defmethod cl-ds:morep ((range vector-range))
  (range-not-fully-read range))


(defmethod cl-ds:peek-front ((range vector-range))
  (if (range-not-fully-read range)
      (values (~> range read-vector (aref (access-lower-bound range)))
              t)
      (values nil nil)))


(defmethod cl-ds:consume-front ((range vector-range))
  (bind (((:values value more) (cl-ds:peek-front range)))
    (when more
      (incf (access-lower-bound range)))
    (values value more)))


(defmethod cl-ds:clone ((range vector-range))
  (make 'vector-range :vector (read-vector range)))


(defmethod cl-ds:peek-back ((range vector-range))
  (if (range-not-fully-read range)
      (values (~> range read-vector (aref (1- (access-upper-bound range))))
              t)
      (values nil nil)))


(defmethod cl-ds:consume-back ((range vector-range))
  (bind (((:values value more) (cl-ds:peek-back range)))
    (when more
      (decf (access-upper-bound range)))
    (values value more)))


(defmethod cl-ds:traverse (function (range vector-range))
  (bind (((:slots %lower-bound %upper-bound %vector) range))
    (iterate
      (for i from %lower-bound below %upper-bound)
      (funcall function (aref %vector i)))
    range))


(defmethod cl-ds:size ((range vector-range))
  (bind (((:slots %lower-bound %upper-bound) range))
    (- %upper-bound %lower-bound)))
