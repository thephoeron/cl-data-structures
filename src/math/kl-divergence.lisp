(in-package #:cl-data-structures.math)


(defclass kl-divergence-matrix-function (cl-ds.alg.meta:multi-aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric kl-divergence-matrix (range &rest fields)
  (:generic-function-class kl-divergence-matrix-function)
  (:method (range &rest fields)
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'kl-divergence-matrix
                                               :fields fields
                                               :key #'identity)))


(defclass info-field ()
  ((%name :initarg :name
          :reader read-name)
   (%data :initarg :data
          :reader read-data)
   (%test :initarg :test
          :reader read-test)
   (%selector-function :initarg :selector-function
                       :reader read-selector-function
                       :initform #'identity)))


(defun continuesp (field)
  (eq (cl-ds:at field :type)
      :continues))


(defun calculate-kl-divergence-between (field1 field2)
  (bind ((table1 (make-hash-table :test (read-test field1)))
         (table2 (make-hash-table :test (read-test field2)))
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


(defun calculate-kl-divergence (fields)
  (iterate
    (with result = (cl-ds.utils:make-half-matrix 'single-float
                                                 (length fields)
                                                 :query-key
                                                 (iterate
                                                   (with table = (make-hash-table))
                                                   (for field in fields)
                                                   (for i from 0)
                                                   (setf (gethash (read-name field) table)
                                                         i)
                                                   (finally (return (rcurry #'gethash table))))))
    (for sublist on fields)
    (for field = (first sublist))
    (iterate
      (for future-field in (rest sublist))
      (setf (cl-ds.utils:mref result (read-name field) (read-name future-field))
            (calculate-kl-divergence-between field future-field)))
    (finally (return result))))


(defun partition-points (length)
  (bind ((number-of-points (max length 10))
         (shift (/ length number-of-points))
         (result (make-array 10 :element-type 'fixnum
                                :adjustable t
                                :fill-pointer 0)))
    (iterate
      (for i from 0 below number-of-points)
      (for offset = (round (* i shift)))
      (vector-push-extend offset result)
      (finally (return result)))))


(defun discrete-form (field data)
  (bind ((sorted (~>> data
                      (map 'vector (or (cl-ds:at field :key) #'identity))
                      (sort _ #'<)))
         (partition-points (partition-points (length data)))
         (key (or (cl-ds:at field :key) #'identity))
         (partitions (map-into (make-array 10 :adjustable t :fill-pointer 0)
                               (curry #'aref sorted)
                               partition-points)))
    (map '(vector fixnum)
         (lambda (x) (cl-ds.utils:lower-bound partitions
                                         (funcall key x)
                                         #'<))
         data)))


(defun initialize-fields (fields data)
  (mapcar (lambda (field)
            (if (continuesp field)
                (make 'info-field
                      :name (cl-ds:at field :name)
                      :test 'eql
                      :data (discrete-form field data)
                      :selector-function #'identity)
                (make 'info-field
                      :name (cl-ds:at field :name)
                      :test (or (cl-ds:at field :test) 'equal)
                      :data data
                      :selector-function (cl-ds:at field :key))))
          fields))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function kl-divergence-matrix-function)
     &rest all
     &key key fields
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (~> (initialize-fields fields vector)
              calculate-kl-divergence))))
