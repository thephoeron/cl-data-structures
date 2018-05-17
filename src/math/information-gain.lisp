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


(defclass info-field ()
  ((%name :initarg :name
          :reader read-name)
   (%type :initarg :type
          :reader read-type)
   (%data :initarg :data
          :reader read-data)
   (%selector-function :initarg :selector-function
                       :reader read-selector-function
                       :initform #'identity)))


(defun continuesp (field)
  (eq (read-type field)
      :continues))


(defun calculate-information-gain-between (field1 field2)
  (bind ((table1 (make-hash-table))
         (table2 (make-hash-table))
         (table3 (make-hash-table :test 'equal))
         (vector1 (read-data field1))
         (vector2 (read-data field2))
         (length (length vector1))
         ((:dflet normalize-table (table))
          (iterate
            (for (key value) in-hashtable table)
            (setf (gethash key table) (/ value length)))))
    (iterate
      (with function1 = (read-selector-function field1))
      (with function2 = (read-selector-function field2))
      (for v1 in-vector vector1)
      (for v2 in-vector vector2)
      (for value1 = (funcall function1 v1))
      (for value2 = (funcall function2 v2))
      (incf (gethash value1 table1 0))
      (incf (gethash value2 table2 0))
      (incf (gethash (cons value1 value2) table3 0)))
    (normalize-table table1)
    (normalize-table table2)
    (iterate
      (for (key p3) in-hashtable table3)
      (for p1 = (gethash (car key) table1))
      (for p2 = (gethash (cdr key) table2))
      (sum (* p1 (log (/ p1 p2)))))))


(defun calculate-information-gain (field fields)
  (declare (type list fields))
  (iterate
    (with result = (make-hash-table :test 'eq))
    (for other-field in fields)
    (setf (gethash (read-name other-field) result)
          (calculate-information-gain-between field other-field))
    (finally (return result))))


(defun calculate-information-gains (fields)
  (declare (type list fields))
  (iterate
    (with result = (make-hash-table :test 'eq))
    (iterate
      (for sublist on fields)
      (for field = (first sublist))
      (for future-fields = (rest sublist))
      (until (endp future-fields))
      (setf (gethash (read-name field) result)
            (calculate-information-gain field
                                        future-fields))
      (finally (return result)))))


(defun partition-points (length))


(defun discrete-form (field data)
  (bind ((sorted (~>> data
                      (map 'vector (read-selector-function field))
                      (sort _ #'<)))
         (partition-points (partition-points (length data)))
         (partitions (map-into (make-array 10 :adjustable t :fill-pointer 0)
                               (curry #'aref sorted)
                               partition-points)))
    (map '(vector fixnum)
         (lambda (x) (cl-ds.utils:lower-bound partitions x #'<))
         data)))


(defun initialize-fields (fields data)
  (mapcar (lambda (field)
            (if (continuesp field)
                (make 'info-field
                      :name (read-name field)
                      :type (read-type field)
                      :data (discrete-form field data)
                      :selector-function #'identity)
                (make 'info-field
                      :name (read-name field)
                      :type (read-type field)
                      :data data
                      :selector-function (read-selector-function field))))
          fields))


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
          (calculate-information-gains (initialize-fields fields vector)))))
