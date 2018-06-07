(in-package #:cl-data-structures.data-frame)


(defun ensure-dimensionality (object more)
  (unless (= #1=(cl-ds:dimensionality object) #2=(length more))
    (error 'cl-ds:dimensionality-error
           :text (format nil
                         "Passed ~a arguments but data-frame dimensionality is ~a."
                         #2# #1#))))


(defun ensure-in-frame (object more)
  (iterate
    (for m in more)
    (unless (fixnump m)
      (error 'cl:type-error :datum m
                            :expected-type 'non-negative-fixnum)))
  (when (some (curry #'> 0) more)
    (error 'cl-ds:argument-out-of-bounds
           :bounds "Must be non negative."
           :argument 'location
           :value more
           :text "Part of location is negative."))
  (unless (every #'< more #1=(read-sizes object))
    (error 'cl-ds:argument-out-of-bounds
           :bounds #1#
           :value more
           :argument 'location
           :text "No such position in the data frame.")))


(defmethod cl-ds:at ((object data-frame) location &rest more)
  (let ((more (cons location more)))
    (ensure-dimensionality object more)
    (ensure-in-frame object more)
    (at-data (access-data object)
             more)))


(defmethod (setf cl-ds:at) (new-value (object data-frame) location &rest more)
  (let ((more (cons location more)))
    (ensure-dimensionality object more)
    (ensure-in-frame object more)
    (set-at-data new-value
                 (access-data object)
                 more)))


(defmethod mutate! (data dimension function &rest ranges)
  (bind ((old-instance (access-data data))
         (new-instance (cl-ds:become-transactional old-instance))
         (*active-data* (make-data-accessor data new-instance dimension)))
    todo
    (cl-ds:freeze! old-instance)
    (setf (access-data data) new-instance)
    data))
