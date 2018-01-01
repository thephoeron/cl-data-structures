(in-package #:cl-data-structures.algorithms)


(defclass hash-join-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric hash-join (primary-range primary-key secondary-range-forms
                       &key test join-function key)
  (:generic-function-class hash-join-function)
  (:method (primary-range primary-key secondary-range-forms
            &key (test 'eql) (join-function #'list) key)
    (apply-aggregation-function primary-range #'hash-join
                                :key key
                                :test test
                                :primary-key primary-key
                                :join-function join-function
                                :secondary-range-forms secondary-range-forms)))


(defstruct hash-join-function-state table ranges keys join-function primary-key)


(defmethod state-result ((function hash-join-function)
                         state)
  (bind (((:struct hash-join-function-state- table ranges keys join-function) state))
    (iterate
      (for range in-vector ranges)
      (for key in-vector keys)
      (for i from 1)
      (cl-ds:traverse (lambda (x)
                        (nest
                         (let ((maybe-vector (gethash (funcall key x) table))))
                         (unless (null maybe-vector))
                         (let ((effective-vector (aref maybe-vector i))))
                         (vector-push-extend x effective-vector)))
                      range))
    (bind ((result (let ((size 0))
                     (maphash (lambda (key value)
                                (declare (ignore key))
                                (incf size (reduce #'+ value :key #'length)))
                              table)
                     (make-array size :element-type t :adjustable t :fill-pointer 0)))
           (collector (compose (rcurry #'vector-push-extend result)
                               join-function)))
      (maphash
       (lambda (key value)
         (declare (ignore key))
         (unless (some #'emptyp value)
           (cl-ds.utils:cartesian value collector)))
       table)
      result)))


(defmethod make-state ((function hash-join-function)
                       &rest all
                       &key
                         test
                         primary-key
                         join-function
                         secondary-range-forms
                       &allow-other-keys)
  (declare (ignore all))
  (make-hash-join-function-state :table (make-hash-table :test test)
                                 :ranges (map 'vector #'car secondary-range-forms)
                                 :primary-key primary-key
                                 :keys (map 'vector #'cdr secondary-range-forms)
                                 :join-function join-function))


(defmethod aggregate ((function hash-join-function)
                      state
                      element)
  (bind (((:struct hash-join-function-state- table ranges primary-key) state)
         (key (funcall primary-key element)))
   (vector-push-extend
    element
    (aref (ensure (gethash key table)
            (with-vectors ((result (~> ranges length 1+ make-array)))
              (iterate
                (for i from 0 below (~> ranges length 1+))
                (setf (result i) (vect)))
              result))
          0))))
