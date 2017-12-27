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
  (with-slots (table ranges keys join-function primary-key) state
    (let* ((result (vect))
           (collector (compose (rcurry #'vector-push-extend result) join-function)))
      (iterate
        (for range in-vector ranges)
        (for key in-vector keys)
        (for i from 1)
        (cl-ds:traverse (lambda (x)
                          (let ((maybe-vector (gethash (funcall key x) table)))
                            (unless (null maybe-vector)
                              (let ((effective-vector (aref maybe-vector i)))
                                (vector-push-extend x effective-vector)))))
                        range))
      (maphash
       (lambda (key value)
         (declare (ignore key))
         (cl-ds.utils:cartesian value collector))
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
  (with-slots (table ranges keys join-function primary-key) state
    (let ((key (funcall primary-key element)))
      (vector-push-extend element
                          (aref (ensure (gethash key table)
                                  (with-vectors ((result (make-array (1+ (length ranges)))))
                                    (iterate
                                      (for i from 0 below (1+ (length ranges)))
                                      (setf (result i) (vect)))
                                    result))
                                0)))))
