(in-package #:cl-data-structures.data-frame)


(defun ensure-dimensionality (object more)
  (unless (= #1=(cl-ds:dimensionality object) #2=(length more))
    (error 'cl-ds:dimensionality-error
           :text (format nil
                         "Passed ~a arguments but data-frame dimensionality is ~a."
                         #2# #1#))))


(defun ensure-in-frame (object more)
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


(labels ((impl (data location)
           (if (endp location)
               data
               (impl (cl-ds:at data
                               (first location))
                     (rest location)))))
  (defmethod cl-ds:at ((object data-frame) location &rest more)
    (let ((more (cons location more)))
      (ensure-dimensionality object more)
      (ensure-in-frame object more)
      (impl (access-data object)
            more))))


(labels ((impl (new-value data location)
           (if (endp (rest location))
               (setf (cl-ds:at data (first location))
                     new-value)
               (impl new-value
                     (cl-ds:at data
                               (first location))
                     (rest location)))))
  (defmethod (setf cl-ds:at) (new-value (object data-frame) location &rest more)
    (let ((more (cons location more)))
      (ensure-dimensionality object more)
      (ensure-in-frame object more)
      (impl new-value
            (access-data object)
            more))))
