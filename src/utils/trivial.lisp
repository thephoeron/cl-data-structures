(in-package #:cl-data-structures.utils)


(defmacro todo (&optional (description "Not implemented!"))
  `(error 'cl-ds:not-implemented 
          :text ,description))
