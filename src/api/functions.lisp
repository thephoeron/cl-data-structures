(in-package #:cl-data-structures)


(defun send (value)
  (funcall *traverse-callback* value))
