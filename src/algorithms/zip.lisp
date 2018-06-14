(in-package #:cl-data-structures.algorithms)


(defclass forward-zipped-ranges (cl-ds:fundamental-forward-range)
  ((%ranges :initarg :ranges)
   (%function :initarg :function)))


(defclass bidirectional-zipped-ranges (forward-zipped-ranges
                                       cl-ds:fundamental-bidirectional-range)
  ())


(defclass random-access-zipped-ranges (bidirectional-zipped-ranges
                                       cl-ds:fundamental-random-access-range)
  ())


(defun init-zipped-ranges (obj)
  (bind (((:slots %ranges) obj))
    (map nil #'cl-ds:reset! %ranges)
    obj))


(defmethod cl-ds:reset! ((range forward-zipped-ranges))
  (reinitialize-instance range))


(defmethod reinitialize-instance :after ((range forward-zipped-ranges) &key &allow-other-keys)
  (init-zipped-ranges range))


(defmethod initialize-instance :after ((range forward-zipped-ranges) &key &allow-other-keys)
  (init-zipped-ranges range))


(defun zip (function &rest ranges)
  (let ((type (common-fundamental-range-class ranges)))
    (assert type)
    (make (eswitch (type)
            ('fundamental-forward-range 'forward-zipped-ranges)
            ('fundamental-bidirectional-range 'bidirectional-zipped-ranges)
            ('fundamental-random-access-range 'random-access-zipped-ranges))
          :ranges ranges
          :function function)))


(defmethod cl-ds:consume-front ((range forward-zipped-ranges))
  (block nil
    (bind (((:slots %function %ranges) range))
      (values (apply %function
                     (mapcar (lambda (x)
                               (multiple-value-bind (value more)
                                   (cl-ds:consume-front x)
                                 (when (null more)
                                   (return-from nil (values nil nil)))
                                 value))
                             %ranges))
              t))))


(defmethod cl-ds:consume-back ((range bidirectional-zipped-ranges))
  (block nil
    (bind (((:slots %function %ranges) range))
      (values (apply %function
                     (mapcar (lambda (x)
                               (multiple-value-bind (value more)
                                   (cl-ds:consume-back x)
                                 (when (null more)
                                   (return-from nil (values nil nil)))
                                 value))
                             %ranges))
              t))))


(defmethod cl-ds:peek-front ((range forward-zipped-ranges))
  (block nil
    (bind (((:slots %function %ranges) range))
      (values (apply %function
                     (mapcar (lambda (x)
                               (multiple-value-bind (value more)
                                   (cl-ds:peek-front x)
                                 (when (null more)
                                   (return-from nil (values nil nil)))
                                 value))
                             %ranges))
              t))))


(defmethod cl-ds:peek-back ((range bidirectional-zipped-ranges))
  (block nil
    (bind (((:slots %function %ranges) range))
      (values (apply %function
                     (mapcar (lambda (x)
                               (multiple-value-bind (value more)
                                   (cl-ds:peek-back x)
                                 (when (null more)
                                   (return-from nil (values nil nil)))
                                 value))
                             %ranges))
              t))))


(defmethod cl-ds:at ((range random-access-zipped-ranges) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (block nil
    (bind (((:slots %function %ranges) range))
      (values (apply %function
                     (mapcar (lambda (x)
                               (multiple-value-bind (value more)
                                   (cl-ds:at x location)
                                 (when (null more)
                                   (return-from nil (values nil nil)))
                                 value))
                             %ranges))
              t))))


(defmethod cl-ds:size ((range random-access-zipped-ranges))
  (bind (((:slots %ranges) range))
    (iterate
      (for range in %ranges)
      (minimize (cl-ds:size range)))))

