(in-package #:cl-data-structures.documentation)
(cl-lore.api.syntax:define-save-output-function save-output-impl)


(defun save-output (&optional path)
  (save-output-impl (build-document) path))
