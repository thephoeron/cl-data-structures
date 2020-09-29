(cl:in-package #:cl-data-structures.algorithms)


(defclass shuffled-range (cl-ds:chunking-mixin
                          cl-ds:fundamental-forward-range)
  ((%table :initform (make-hash-table)
           :initarg :table
           :reader read-table)
   (%index :accessor access-index
           :initarg :index)
   (%from :reader read-from
          :initarg :from)
   (%to :reader read-to
        :initarg :to))
  (:metaclass funcallable-standard-class))


(defun shuffled-range (from to)
  (check-type from integer)
  (check-type to integer)
  (unless (<= from to)
    (error 'cl-ds:incompatible-arguments
           :format-control "FROM must be smaller then TO."
           :parameters '(from to)
           :values (list from to)))
  (make 'shuffled-range
        :index from
        :from from
        :to to))


(defmethod cl-ds:consume-front ((object shuffled-range))
  (bind (((:slots %table %index %from %to) object))
    (cond ((eql (- %to 1) %index)
           (let ((index %index))
             (ensure (gethash %index %table) %index)
             (incf %index)
             (values (gethash index %table) t)))
          ((< %index %to)
           (let ((next-random (random-in-range %index %to))
                 (index %index))
             (ensure (gethash %index %table) %index)
             (ensure (gethash next-random %table) next-random)
             (rotatef (gethash %index %table)
                      (gethash next-random %table))
             (incf %index)
             (values (gethash index %table) t)))
          (t (values nil nil)))))


(defmethod cl-ds:peek-front ((object shuffled-range))
  (bind (((:slots %table %index %from %to) object))
    (cond ((eql (- %to 1) %index)
           (values (ensure (gethash %index %table) %index) t))
          ((< %index %to)
           (let ((next-random (random-in-range %index %to)))
             (ensure (gethash %index %table) %index)
             (ensure (gethash next-random %table) next-random)
             (rotatef (gethash %index %table)
                      (gethash next-random %table))
             (values (gethash (1+ %index) %table) t)))
          (t (values nil nil)))))


(defmethod cl-ds:clone ((object shuffled-range))
  (make 'shuffled-range
        :index (access-index object)
        :from (access-index object)
        :table (copy-hash-table (read-table object))
        :to (read-to object)))


(defmethod cl-ds:reset! ((object shuffled-range))
  (setf (access-index object) (read-from object))
  object)
