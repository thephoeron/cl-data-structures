(in-package #:cl-data-structures.data-frame)


(defmacro data (&rest locations)
  `(cl-ds:at *active-data* ,@locations))
