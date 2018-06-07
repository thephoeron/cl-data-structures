(in-package #:cl-data-structures.data-frame)


(defmacro cell (&rest locations)
  `(at-cell *active-data* ,@locations))
