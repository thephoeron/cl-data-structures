(cl:in-package #:cl-ds.adapters)


(defmethod cl-ds:at ((matrix cl-ds.utils:half-matrix) location &rest more)
  (let ((first location)
        (second (first more)))
    (cl-ds:assert-one-dimension (rest more))
    (cl-ds.utils:mref matrix first second)))
