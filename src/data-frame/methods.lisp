(in-package #:cl-data-structures.data-frame)


(defun ensure-dimensionality (object more)
  (unless (= #1=(cl-ds:dimensionality object) #2=(~> more length 1+))
    (error 'cl-ds:dimensionality-error
           :text (format nil
                         "Passed ~a arguments but data-frame dimensionality is ~a."
                         #1# #2#))))


(defmethod cl-ds:at ((object data-frame) location &rest more)
  (ensure-dimensionality object more)
  (labels ((impl (data location)
             (if (endp location)
                 data
                 (impl (cl-ds:at data
                                 (first location))
                       (rest location)))))
    (impl (access-data object)
          (cons location more))))


(defmethod (setf cl-ds:at) (new-value (object data-frame) location &rest more)
  (ensure-dimensionality object more)
  (labels ((impl (data location)
             (if (endp (rest location))
                 (setf (cl-ds:at data (first location))
                       new-value)
                 (impl (cl-ds:at data
                                 (first location))
                       (rest location)))))
    (impl (access-data object)
          (cons location more))))
