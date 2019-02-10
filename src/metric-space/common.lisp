(in-package #:cl-data-structures.metric-space)


(defgeneric same (container bucket element)
  (:method ((container metric-space-set)
            (bucket t)
            (element t))
    (funcall (read-same-fn container) bucket element)))


(defgeneric distance (container bucket element)
  (:method ((container metric-space-set)
            (bucket t)
            (element t))
    (funcall (read-metric-fn container) bucket element)))


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
                cl-ds.common:empty-eager-modification-operation-status)
        (let ((old-value (aref bucket position)))
          (cl-ds.utils:swapop bucket position)
          (values bucket
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   old-value
                   t))))))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:put!-function)
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
                  cl-ds.common:empty-changed-eager-modification-operation-status))
        (values bucket
                (cl-ds.common:make-eager-modification-operation-status
                 t
                 (aref bucket position)
                 t)))))


(defmethod cl-ds.meta:make-bucket ((operation t)
                                   (container metric-space-set)
                                   location
                                   &rest all)
  (declare (ignore all))
  (values (vect (cl-ds:force location))
          (cl-ds.common:make-eager-modification-operation-status nil nil t)))


(defmethod cl-ds.meta:make-bucket-from-multiple ((operation t)
                                                 (container metric-space-set)
                                                 data
                                                 &rest all)
  (declare (ignore all))
  (setf data (cl-ds:force data))
  (values
   (if (and (eq (type-of data) 'vector) (array-has-fill-pointer-p data))
       data
       (lret ((result (vect)))
         (cl-ds:traverse data (rcurry #'vector-push-extend result))))
   (cl-ds.common:make-eager-modification-operation-status nil nil t)))
