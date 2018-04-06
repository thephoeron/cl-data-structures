(in-package #:cl-data-structures.metric-space)


(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:erase!-function)
                                      (container mutable-metric-space-set)
                                      bucket
                                      location
                                      &rest all)
  (declare (ignore all))
  (setf location (cl-ds:force location))
  (bind ((position (position location bucket
                             :test (curry #'cl-ds.common.egnat:same
                                          container))))
    (if (null position)
        (values bucket
                cl-ds.common:empty-eager-modification-operation-status
                nil)
        (let ((old-value (aref bucket position)))
          (cl-ds.utils:swapop bucket position)
          (values bucket
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   old-value)
                  t)))))


(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:erase-if!-function)
                                      (container mutable-metric-space-set)
                                      bucket
                                      location
                                      &rest all
                                      &key condition-fn)
  (declare (ignore all))
  (setf location (cl-ds:force location))
  (bind ((position (position location bucket
                             :test (curry #'cl-ds.common.egnat:same
                                          container))))
    (if (null position)
        (values bucket
                cl-ds.common:empty-eager-modification-operation-status
                nil)
        (bind ((old-value (aref bucket position))
               (test-passed (funcall condition-fn old-value location)))
          (if test-passed
              (progn
                (cl-ds.utils:swapop bucket position)
                (values bucket
                        (cl-ds.common:make-eager-modification-operation-status
                         t
                         old-value)
                        t))
              (values bucket
                      (cl-ds.common:make-eager-modification-operation-status
                       t
                       old-value)
                      nil))))))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:insert!-function)
                                    (container mutable-metric-space-set)
                                    bucket
                                    location
                                    &rest all)
  (declare (ignore all))
  (setf location (cl-ds:force location))
  (bind ((position (position location bucket
                             :test (curry #'cl-ds.common.egnat:same
                                          container))))
    (if (null position)
        (progn
          (vector-push-extend location bucket)
          (values bucket
                  cl-ds.common:empty-eager-modification-operation-status
                  t))
        (values bucket
                (cl-ds.common:make-eager-modification-operation-status
                 t
                 (shiftf (aref bucket position) location))
                t))))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:add!-function)
                                    (container mutable-metric-space-set)
                                    bucket
                                    location
                                    &rest all)
  (declare (ignore all))
  (setf location (cl-ds:force location))
  (bind ((position (position location bucket
                             :test (curry #'cl-ds.common.egnat:same
                                          container))))
    (if (null position)
        (progn
          (vector-push-extend location bucket)
          (values bucket
                  cl-ds.common:empty-eager-modification-operation-status
                  t))
        (values bucket
                cl-ds.common:empty-eager-modification-operation-status
                nil))))


(defmethod cl-ds.meta:make-bucket ((operation t)
                                   (container metric-space-set)
                                   location
                                   &rest all)
  (declare (ignore all))
  (vect (cl-ds:force location)))


(defmethod cl-ds.meta:make-bucket-from-multiple ((operation t)
                                                 (container metric-space-set)
                                                 data
                                                 &rest all)
  (declare (ignore all))
  (setf data (cl-ds:force data))
  (if (and (eq (type-of data) 'vector) (array-has-fill-pointer-p data))
      data
      (lret ((result (vect)))
        (cl-ds:traverse (curry #'vector-push-extend result) data))))
