(in-package :cl-ds.adapters)

(defmethod cl-ds:at ((container hash-table) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (gethash location container))
