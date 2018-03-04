(in-package #:cl-data-structures.adapters)


(defmethod cl-ds:size ((seq list))
  (length seq))
