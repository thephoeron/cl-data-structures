(in-package #:cl-data-structures.math)


(defclass information-gain-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric information-gain (range &rest fields &key key)
  (:generic-function-class information-gain-function)
  (:method (range &rest fields &key key)
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'information-gain
                                               :fields fields
                                               :key key)))


(defun calculate-information-gain-between (vector field other-field)
  (declare (type (vector vector)
                 (list field other-field))))


(defun calculate-information-gain (vector field fields)
  (declare (type (vector vector)
                 (list fields)
                 (list field)))
  (iterate
    (with result = (make-hash-table :test 'eq))
    (for other-field in fields)
    (setf (gethash (first other-field) result)
          (calculate-information-gain-between vector field other-field))
    (finally (return result))))


(defun calculate-information-gains (vector fields)
  (declare (type (vector vector)
                 (list fields)))
  (iterate
    (with result = (make-hash-table :test 'eq))
    (iterate
      (for sublist on fields)
      (for field = (first sublist))
      (for (field-name field-class select-function) = field)
      (for future-fields = (rest sublist))
      (collect field into past-fields at start)
      (setf (gethash field-name result)
            (calculate-information-gain vector
                                        field
                                        (append future-fields past-fields)))
      (finally (return result)))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function information-gain-function)
     &rest all
     &key key fields
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (calculate-information-gain vector fields))))
